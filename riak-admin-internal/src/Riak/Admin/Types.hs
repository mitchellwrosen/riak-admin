module Riak.Admin.Types
  ( BucketProps
  , BucketType
  , DataType(..)
  , FunctionName
  , Quorum(..)
  ) where

import Riak.Admin.Types.BucketProps
import Riak.Admin.Types.Internal

import Data.Text (Text)

type BucketType = Text
