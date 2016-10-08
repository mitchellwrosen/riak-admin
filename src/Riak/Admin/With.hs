{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Riak.Admin.With
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
import Data.Default.Class
import Data.Monoid
import Data.Proxy
import Data.Reflection     (Reifies(..), reify)
import Data.Text           (Text)


-- | Unexported IO newtype with a MonadShell instance that prepends the
-- reflected Text.
newtype IO' s a = IO' { unIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance Reifies s Text => MonadShell (IO' s) where
  shell :: Text -> IO' s Text
  shell s = IO' (shell (prefix <> " " <> s))
   where
    prefix :: Text
    prefix = reflect (Proxy :: Proxy s)


bucketTypeCreate :: Text -> BucketType -> BucketProps -> IO ()
bucketTypeCreate s a b = reify s go
 where
  go :: forall s. Reifies s Text => Proxy s -> IO ()
  go _ = unIO (BucketType.bucketTypeCreate a b :: IO' s _)

bucketTypeList :: Text -> IO [(BucketType, Active)]
bucketTypeList s = reify s go
 where
  go :: forall s. Reifies s Text => Proxy s -> IO [(BucketType, Active)]
  go _ = unIO (BucketType.bucketTypeList :: IO' s _)
