module Riak.Admin.Command.BucketType
  ( BucketType
  , Active(..)
  , bucketTypeCreate
  , bucketTypeList
  ) where

import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import Control.Exception.Safe
import Control.Monad.Shell
import Data.Char              (isSpace)
import Data.Text              (Text)
import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Encoding as LText

data Active
  = Active
  | NotActive
  deriving (Eq, Ord, Show)

bucketTypeList :: (MonadShell m, MonadThrow m) => m [(BucketType, Active)]
bucketTypeList = do
  output <- shell "riak-admin bucket-type list"
  mapM parseOutput (Text.lines output)
 where
  parseOutput :: MonadThrow m => Text -> m (BucketType, Active)
  parseOutput output =
    case runParser parser "riak-admin bucket-type list" output of
      Left err -> throw err
      Right x -> pure x

  parser :: Parser (BucketType, Active)
  parser = do
    typ <- many (satisfy (not . isSpace))
    _ <- spaceChar
    active <-
      between (char '(') (char ')')
        (Active    <$ string "active" <|>
         NotActive <$ string "not active")
    pure (Text.pack typ, active)

bucketTypeCreate
  :: (MonadShell m, MonadThrow m) => BucketType -> BucketProps -> m ()
bucketTypeCreate typ props = do
  _ <-
    shellf "riak-admin bucket-type create {} '{}'"
      (typ, LText.decodeUtf8 (encodeBucketProps props))
  pure ()
