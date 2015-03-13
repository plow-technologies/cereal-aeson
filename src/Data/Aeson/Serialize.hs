{-# LANGUAGE OverloadedStrings #-} -- For JSON field names
{-|
 - This module defines a newtype wrapper for instances of Serialize which is an instance of
 - FromJSON and ToJSON. The JSON representation is an object with a single field "binarydata"
 - whose value is a base64 encoding of the serialized bytestring.
|-}

module Data.Aeson.Serialize (SerializeToJSON(..), base64TextToSerialize, serializeToBase64Text) where

import           Control.Applicative            ((<$>))
import           Data.Monoid                    (mempty)
import           Data.Aeson                     (FromJSON(..), ToJSON(..), (.=), (.:), object, withObject)
import           Data.Aeson.Types               (modifyFailure)
import           Data.ByteString.Lazy           (fromStrict, toStrict)
import qualified Data.ByteString.Base64  as B64 (encode, decode)
import           Data.Text.Lazy                 (Text)
import           Data.Text.Lazy.Encoding        (encodeUtf8, decodeUtf8)
import           Data.Serialize                 (Serialize(..))
import qualified Data.Serialize          as S   (encode, decode)

-- | Parse a Serialize something from its base64 encoding as Text
base64TextToSerialize :: (Serialize a) => Text -> Either String a
base64TextToSerialize serializedText = (B64.decode $ toStrict $ encodeUtf8 serializedText) >>= S.decode

-- | Serialize a Serialize something, base64 encode it, and return the result as Text
serializeToBase64Text :: (Serialize a) => a -> Text
serializeToBase64Text = decodeUtf8 . fromStrict . B64.encode . S.encode

-- | Newtype wrapper for Serialize instances which has ToJSON and FromJSON instances
newtype SerializeToJSON a = SerializeToJSON a deriving (Read, Show, Ord, Eq)

instance Serialize a => FromJSON (SerializeToJSON a) where
  parseJSON = withObject "Serialized data must be backed as an object" (\obj -> base64TextToSerialize <$> (obj .: "binarydata") >>= either (\err -> modifyFailure (const err) mempty) (return . SerializeToJSON) )

instance Serialize a => ToJSON (SerializeToJSON a) where
  toJSON (SerializeToJSON thing) = object [ "binarydata" .= serializeToBase64Text thing ]
