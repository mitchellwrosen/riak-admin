{-# LANGUAGE UndecidableInstances #-}

module Riak.Admin.Given
  ( -- * bucket-type
    BucketType
  , BucketProps(..)
  , Active(..)
  , bucketTypeCreate
  , bucketTypeList
    -- * Re-exports
  , module Data.Default.Class
  ) where

import Control.Monad.Shell
import Riak.Admin.Command.BucketType (Active(..), BucketType)
import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import qualified Riak.Admin.Command.BucketType as BucketType

import Control.Monad.Catch (MonadThrow)
import Data.Monoid
import Data.Reflection     (Given, given)
import Data.Text           (Text)
import System.Exit         (ExitCode)

import Data.Default.Class

-- | Unexported IO newtype with a MonadShell instance that prepends the Given
-- Text.
newtype IO' a = IO' { unIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance Given Text => MonadShell IO' where
  shellCode :: Text -> IO' (ExitCode, Text, Text)
  shellCode s = IO' (shellCode (given <> " " <> s))

bucketTypeCreate :: Given Text => BucketType -> BucketProps -> IO ()
bucketTypeCreate a b = unIO (BucketType.bucketTypeCreate a b)

bucketTypeList :: Given Text => IO [(BucketType, Active)]
bucketTypeList = unIO BucketType.bucketTypeList
