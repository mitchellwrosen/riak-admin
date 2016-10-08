module Control.Monad.Shell where

import Control.Concurrent.Async
import Control.Exception.Safe
import Data.Text                (Text)
import Data.Text.Buildable      (Buildable)
import Data.Text.Format         (Format, Only(..), format)
import Data.Text.Format.Params  (Params)
import System.Exit              (ExitCode(..))
import System.IO                (Handle)
import System.Process           (ProcessHandle)

import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified Data.Text.Lazy as LText
import qualified System.Process as Process


data ShellFailure
  = ShellFailure Text Int Text Text
  -- ^ Shell command failed with non-zero exit code
  deriving Show

instance Exception ShellFailure


class MonadThrow m => MonadShell m where
  shell :: Text -> m Text

shellf :: (Params ps, MonadShell m) => Format -> ps -> m Text
shellf fmt ps = shell (LText.toStrict (format fmt ps))

shellf1 :: (Buildable a, MonadShell m) => Format -> a -> m Text
shellf1 fmt x = shellf fmt (Only x)


instance MonadShell IO where
  shell :: Text -> IO Text
  shell s = do
    let acquire :: IO (Handle, Handle, ProcessHandle)
        acquire = do
          (Nothing, Just hout, Just herr, ph) <-
            Process.createProcess ((Process.shell (Text.unpack s))
              { Process.std_in  = Process.NoStream
              , Process.std_out = Process.CreatePipe
              , Process.std_err = Process.CreatePipe
              })
          pure (hout, herr, ph)

        release :: (Handle, Handle, ProcessHandle) -> IO ()
        release (_, _, ph) = Process.terminateProcess ph

        action :: (Handle, Handle, ProcessHandle) -> IO (ExitCode, Text, Text)
        action (hout, herr, ph) = runConcurrently $ (,,)
          <$> Concurrently (Process.waitForProcess ph)
          <*> Concurrently (Text.hGetContents hout)
          <*> Concurrently (Text.hGetContents herr)

    bracket acquire release action >>= \case
      (ExitSuccess, out, _) -> pure out
      (ExitFailure n, out, err) -> throw (ShellFailure s n out err)
