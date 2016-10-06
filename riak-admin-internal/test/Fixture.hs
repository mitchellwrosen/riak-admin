module Fixture
  ( ShellDict(..)
  , Shell
  , runShell
  , unShell
  , logShell
  , log
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.RWS
import Control.Monad.Shell
import Data.DList          (DList)
import Data.Text           (Text)
import System.Exit         (ExitCode)
import Prelude             hiding (log)

import qualified Data.DList as DList

data ShellDict s = ShellDict
  { _shellCode :: Text -> Shell s (ExitCode, Text, Text) }

-- IO for throwing exceptions; does not derive MonadIO
newtype Shell s a = Shell
  { _unShell :: RWST (ShellDict s) (DList Text) s IO a }
  deriving (Functor, Applicative, Monad, MonadReader (ShellDict s),
             MonadWriter (DList Text), MonadState s, MonadThrow)

instance MonadShell (Shell s) where
  shellCode :: Text -> Shell s (ExitCode, Text, Text)
  shellCode cmd = do
    ShellDict{..} <- ask
    _shellCode cmd

runShell :: Shell s a -> ShellDict s -> s -> IO (a, s, [Text])
runShell (Shell m) dict s = do
  (a, s', w) <- runRWST m dict s
  pure (a, s', DList.toList w)

unShell :: Shell () a -> ShellDict () -> IO a
unShell s dict = do
  (a, _, _) <- runShell s dict ()
  pure a

logShell :: Shell () a -> ShellDict () -> IO [Text]
logShell s dict = do
  (_, _, w) <- runShell s dict ()
  pure w

log :: Text -> Shell s ()
log w = tell (pure w)
