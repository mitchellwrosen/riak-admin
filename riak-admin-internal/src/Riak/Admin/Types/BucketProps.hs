module Riak.Admin.Types.BucketProps where

import Riak.Admin.Types.Internal

import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Text (Text)

data BucketProps = BucketProps
  { allow_mult      :: Maybe Bool
  , basic_quorum    :: Maybe Bool
  , big_vclock      :: Maybe Int
  , datatype        :: Maybe DataType
  , dvv_enabled     :: Maybe Bool
  , dw              :: Maybe Quorum
  , last_write_wins :: Maybe Bool
  , n_val           :: Maybe Int
  , notfound_ok     :: Maybe Bool
  , old_vclock      :: Maybe Int
  , postcommit      :: Maybe [FunctionName]
  , pr              :: Maybe Quorum
  , precommit       :: Maybe [FunctionName]
  , pw              :: Maybe Quorum
  , r               :: Maybe Quorum
  , rw              :: Maybe Quorum
  , small_vclock    :: Maybe Int
  , w               :: Maybe Quorum
  , young_vclock    :: Maybe Int
  }

instance Default BucketProps where
  def = BucketProps Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing

encodeBucketProps :: BucketProps -> ByteString
encodeBucketProps BucketProps{..} =
  encodingToLazyByteString (wrapObject (text "props" >< colon >< fields))
 where
  fields :: Encoding
  fields = pairs (mconcat
    [ f "allow_mult"      allow_mult
    , f "basic_quorum"    basic_quorum
    , f "big_vclock"      big_vclock
    , f "datatype"        datatype
    , f "dvv_enabled"     dvv_enabled
    , f "dw"              dw
    , f "last_write_wins" last_write_wins
    , f "n_val"           n_val
    , f "notfound_ok"     notfound_ok
    , f "old_vclock"      old_vclock
    , f "postcommit"      postcommit
    , f "pr"              pr
    , f "precommit"       precommit
    , f "pw"              pw
    , f "r"               r
    , f "rw"              rw
    , f "small_vclock"    small_vclock
    , f "w"               w
    , f "young_vclock"    young_vclock
    ])
   where
    f :: ToJSON a => Text -> Maybe a -> Series
    f s = maybe mempty (s .=)
