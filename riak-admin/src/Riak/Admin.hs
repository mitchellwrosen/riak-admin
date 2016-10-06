module Riak.Admin
  ( -- * bucket-type
    BucketType
  , BucketProps(..)
  , bucketTypeCreate
  , bucketTypeList
    -- * Re-exports
  , module Data.Default.Class
  ) where

import Riak.Admin.Command.BucketType (BucketType)
import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import qualified Riak.Admin.Command.BucketType as BucketType

import Data.Default.Class

bucketTypeCreate :: BucketType -> BucketProps -> IO ()
bucketTypeCreate = BucketType.bucketTypeCreate

bucketTypeList :: IO [(BucketType, Bool)]
bucketTypeList = BucketType.bucketTypeList
