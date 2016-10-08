module Riak.Admin.Types
  ( BucketProps
  , BucketType
  , module Riak.Admin.Types.Internal
  ) where

import Riak.Admin.Types.BucketProps
import Riak.Admin.Types.Internal

import Data.Text (Text)

type BucketType = Text
