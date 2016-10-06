module Riak.Admin.Command.BucketType
  ( BucketType
  , bucketTypeCreate
  , bucketTypeList
  ) where

import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import Control.Exception.Safe
import Control.Monad.Shell
import Data.Text              (Text)
import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Data.Text               as Text
import qualified Data.Text.Lazy.Encoding as LText

bucketTypeList :: (MonadShell m, MonadThrow m) => m [(BucketType, Bool)]
bucketTypeList = do
  output <- shell "riak-admin bucket-type list"
  mapM parseOutput (Text.lines output)
 where
  parseOutput :: MonadThrow m => Text -> m (BucketType, Bool)
  parseOutput output =
    case runParser parser "riak-admin bucket-type list" output of
      Left err -> throw err
      Right x -> pure x

  parser :: Parser (BucketType, Bool)
  parser = do
    typ <- many alphaNumChar
    _ <- spaceChar
    active <-
      between (char '(') (char ')')
        (True  <$ string "active" <|>
         False <$ string "not active")
    pure (Text.pack typ, active)

bucketTypeCreate
  :: (MonadShell m, MonadThrow m) => BucketType -> BucketProps -> m ()
bucketTypeCreate typ props = do
  _ <-
    shellf "riak-admin bucket-type create {} '{}'"
      (typ, LText.decodeUtf8 (encodeBucketProps props))
  pure ()
