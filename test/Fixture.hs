module Fixture
  ( ShellDict(..)
  , Shell
  , runShell
  , unShell
  , logShell
  , log
  ) where

import Riak.Admin.Internal (MonadShell(..))

import Control.Exception    (SomeException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Writer
import Data.DList           (DList)
import Data.Text            (Text)
import Lens.Micro
import Prelude              hiding (log)

import qualified Data.DList as DList

data ShellDict = ShellDict
  { _shell :: Text -> Shell Text }

newtype Shell a = Shell
  { _unShell
      :: ReaderT ShellDict
           (WriterT (DList Text)
             (Either SomeException))
               a
  } deriving (Functor, Applicative, Monad, MonadReader ShellDict, MonadThrow,
               MonadWriter (DList Text))

instance MonadShell Shell where
  shell :: Text -> Shell Text
  shell cmd = do
    ShellDict{..} <- ask
    _shell cmd


runShell :: Shell a -> ShellDict -> Either SomeException (a, [Text])
runShell s dict =
  over (_Right . _2) DList.toList (runWriterT (runReaderT (_unShell s) dict))

unShell :: Shell a -> ShellDict -> Either SomeException a
unShell s dict = fst <$> runShell s dict

logShell :: Shell a -> ShellDict -> Either SomeException [Text]
logShell s dict = snd <$> runShell s dict

log :: Text -> Shell ()
log w = tell (pure w)
