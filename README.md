Conversions between cereal and aeson
====================================

This package exports newtype wrappers for Serialize instances and FromJSON and ToJSON instances.

* `Data.Serialize.Aeson` exports a newtype wrapper for instances of FromJSON and ToJSON which
  has an instance for Serialize. This serialize instance puts and gets the underlying datatype
  as JSON-encoded bytestrings.

* `Data.Aeson.Serialize` exports a newtype wrapper for instances of Serialize which has instances
   of FromJSON and ToJSON. The JSON encoding of this newtype has a single field, `binarydata`,
   which contains a base64 encoding of the serialization of the data.
