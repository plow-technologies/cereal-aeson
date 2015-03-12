{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.Serialize (SerializeJSON(..)) where

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

newtype SerializeJSON a = SerializeJSON a deriving (Read, Show, Ord, Eq)

base64TextToSerialize :: (Serialize a) => Text -> Either String a
base64TextToSerialize serializedText = (B64.decode $ toStrict $ encodeUtf8 serializedText) >>= S.decode

serializeToBase64Text :: (Serialize a) => a -> Text
serializeToBase64Text = decodeUtf8 . fromStrict . B64.encode . S.encode

instance Serialize a => FromJSON (SerializeJSON a) where
  parseJSON = withObject "Serialized data must be backed as an object" (\obj -> base64TextToSerialize <$> (obj .: "binarydata") >>= either (\err -> modifyFailure (const err) mempty) (return . SerializeJSON) )

instance Serialize a => ToJSON (SerializeJSON a) where
  toJSON (SerializeJSON thing) = object [ "binarydata" .= serializeToBase64Text thing ]
