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
  | ShellStderr Text Text Text
  -- ^ Shell command wrote to stderr, yet succeeded
  deriving Show

instance Exception ShellFailure


class Monad m => MonadShell m where
  shellCode :: Text -> m (ExitCode, Text, Text)

shellfCode
  :: (Params ps, MonadShell m)
  => Format -> ps -> m (ExitCode, Text, Text)
shellfCode fmt ps = shellCode (LText.toStrict (format fmt ps))

shellf1Code
  :: (Buildable a, MonadShell m)
  => Format -> a -> m (ExitCode, Text, Text)
shellf1Code fmt x = shellfCode fmt (Only x)

shell :: (MonadShell m, MonadThrow m) => Text -> m Text
shell s = do
  (code, out, err) <- shellCode s
  case code of
    ExitSuccess ->
      if Text.null err
        then pure out
        else throw (ShellStderr s out err)
    ExitFailure n -> throw (ShellFailure s n out err)

shellf :: (Params ps, MonadShell m, MonadThrow m) => Format -> ps -> m Text
shellf fmt ps = shell (LText.toStrict (format fmt ps))

shellf1 :: (Buildable a, MonadShell m, MonadThrow m) => Format -> a -> m Text
shellf1 fmt x = shellf fmt (Only x)


instance MonadShell IO where
  shellCode :: Text -> IO (ExitCode, Text, Text)
  shellCode (Text.unpack -> cmd) = do
    let acquire :: IO (Handle, Handle, ProcessHandle)
        acquire = do
          (Nothing, Just hout, Just herr, ph) <-
            Process.createProcess ((Process.shell cmd)
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

    bracket acquire release action
