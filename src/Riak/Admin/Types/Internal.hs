module Riak.Admin.Types.Internal where

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Text           (Text)

data DataType
  = CounterType
  | MapType
  | SetType
  deriving (Eq, Show)

instance ToJSON DataType where
  toJSON :: DataType -> Value
  toJSON = \case
    CounterType -> "counter"
    MapType     -> "map"
    SetType     -> "set"

  toEncoding :: DataType -> Encoding
  toEncoding = \case
    CounterType -> text "counter"
    MapType     -> text "map"
    SetType     -> text "set"

type FunctionName = Text

data Quorum
  = Default
  | One
  | Quorum
  | All
  | N Int
  deriving (Eq, Show)

instance ToJSON Quorum where
  toJSON :: Quorum -> Value
  toJSON = \case
    Default -> "default"
    One     -> "one"
    Quorum  -> "quorum"
    All     -> "all"
    N n     -> toJSON n

  toEncoding :: Quorum -> Encoding
  toEncoding = \case
    Default -> text "default"
    One     -> text "one"
    Quorum  -> text "quorum"
    All     -> text "all"
    N n     -> toEncoding n
