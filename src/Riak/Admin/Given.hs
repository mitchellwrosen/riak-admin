{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Riak.Admin.Given
  ( -- * bucket-type
    BucketType
  , BucketProps(..)
  , Active(..)
  , bucketTypeCreate
  , bucketTypeList
    -- * Re-exports
  , module Data.Default.Class
  , Given(..)
  ) where

import Control.Monad.Shell
import Riak.Admin.Command.BucketType (Active(..), BucketType)
import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import qualified Riak.Admin.Command.BucketType as BucketType

import Control.Monad.Catch (MonadThrow)
import Data.Default.Class
import Data.Monoid
import Data.Reflection     (Given(..))
import Data.Text           (Text)


-- | Unexported IO newtype with a MonadShell instance that prepends the Given
-- Text.
newtype IO' a = IO' { unIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance Given Text => MonadShell IO' where
  shell :: Text -> IO' Text
  shell s = IO' (shell (given <> " " <> s))

bucketTypeCreate :: Given Text => BucketType -> BucketProps -> IO ()
bucketTypeCreate a b = unIO (BucketType.bucketTypeCreate a b)

bucketTypeList :: Given Text => IO [(BucketType, Active)]
bucketTypeList = unIO BucketType.bucketTypeList
