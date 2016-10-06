module Riak.Admin
  ( -- * bucket-type
    BucketType
  , bucketTypeList
  ) where

import Riak.Admin.Command.BucketType (BucketType)

import qualified Riak.Admin.Command.BucketType as BucketType

bucketTypeList :: IO [(BucketType, Bool)]
bucketTypeList = BucketType.bucketTypeList
