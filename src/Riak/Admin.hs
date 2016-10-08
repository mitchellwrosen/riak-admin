{-# LANGUAGE PartialTypeSignatures                #-}
{-# LANGUAGE UndecidableInstances                 #-}

module Riak.Admin
  ( -- * bucket-type
    BucketType
  , BucketProps(..)
  , Active(..)
  , bucketTypeCreate
  , bucketTypeCreate'
  , bucketTypeCreateG
  , bucketTypeList
  , bucketTypeList'
  , bucketTypeListG
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
import Data.Proxy
import Data.Reflection
import Data.Text           (Text)
import System.Exit         (ExitCode)


-- | Unexported IO newtype with a MonadShell instance that prepends the Given
-- Text.
newtype GIO a = GIO { unGIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance Given Text => MonadShell GIO where
  shellCode :: Text -> GIO (ExitCode, Text, Text)
  shellCode s = GIO (shellCode (given <> " " <> s))

-- | Unexported IO newtype with a MonadShell instance that prepends the
-- reflected Text.
newtype RIO s a = RIO { unRIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadThrow)

instance Reifies s Text => MonadShell (RIO s) where
  shellCode :: Text -> RIO s (ExitCode, Text, Text)
  shellCode s = RIO (shellCode (prefix <> " " <> s))
   where
    prefix :: Text
    prefix = reflect (Proxy :: Proxy s)


bucketTypeCreate :: BucketType -> BucketProps -> IO ()
bucketTypeCreate = BucketType.bucketTypeCreate

bucketTypeCreate' :: Text -> BucketType -> BucketProps -> IO ()
bucketTypeCreate' s a b = reify s go
 where
  go :: forall s. Reifies s Text => Proxy s -> IO ()
  go _ = unRIO (BucketType.bucketTypeCreate a b :: RIO s _)

bucketTypeCreateG :: Given Text => BucketType -> BucketProps -> IO ()
bucketTypeCreateG a b = unGIO (BucketType.bucketTypeCreate a b)



bucketTypeList :: IO [(BucketType, Active)]
bucketTypeList = BucketType.bucketTypeList

bucketTypeList' :: Text -> IO [(BucketType, Active)]
bucketTypeList' s = reify s go
 where
  go :: forall s. Reifies s Text => Proxy s -> IO [(BucketType, Active)]
  go _ = unRIO (BucketType.bucketTypeList :: RIO s _)

bucketTypeListG :: Given Text => IO [(BucketType, Active)]
bucketTypeListG = unGIO BucketType.bucketTypeList
