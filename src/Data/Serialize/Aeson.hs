-- Extended from aeson-serialize by Kevin Holt

{-|
 - This module defines a newtype wrapper for instances of FromJSON and ToJSON which
 - is an instance of Serialize. The serialized form is a UTF-8 encoded bytestring
 - of the JSON representation of the datatype.
|-}

module Data.Serialize.Aeson where

import           Data.Aeson
import           Data.Serialize hiding (decode, encode)

-- | put function for all serialization of a json type
putToJSON :: (ToJSON a) => Putter a
putToJSON = put . encode

-- | get function for all deserialization of a json type
getFromJSON :: (FromJSON a) => Get a
getFromJSON = get >>= (either fail return) . eitherDecode

-- | Newtype wrapper for (From,To)JSON instances which has a Serializable
-- instance
newtype JSONToSerialize a = JSONToSerialize a deriving (Read, Show, Ord, Eq)

instance (FromJSON a, ToJSON a) => Serialize (JSONToSerialize a) where
  put (JSONToSerialize x) = putToJSON x
  get = fmap JSONToSerialize getFromJSON
