module Riak.Admin.Command.BucketType
  ( BucketType
  , bucketTypeList
  ) where

import Control.Exception.Safe
import Control.Monad.Shell
import Data.Text              (Text)
import Text.Megaparsec
import Text.Megaparsec.Text

import qualified Data.Text as Text

type BucketType = Text

bucketTypeList :: (MonadShell m, MonadThrow m) => m [(BucketType, Bool)]
bucketTypeList = do
  output <- shell "riak-admin bucket-type list"
  mapM parseOutput (Text.lines output)

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
