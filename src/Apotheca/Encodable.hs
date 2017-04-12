{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Apotheca.Encodable
( -- Re-exports
  S.Serialize (..)
, A.ToJSON (..), A.FromJSON (..)
-- Encodable
, Encodable (..)
, EncodingFormat (..)
, GzipCompression (..)
, Encoding (..)
, (@>)
, encodeWith, decodeWith
, encodeWithFile, decodeWithFile
, encodeBin, decodeBin
, encodeJSON, decodeJSON
, encodeYAML, decodeYAML
, EncodingTest (..)
, encodingTest
, testEncodings
, testAll
) where



import           GHC.Generics

import qualified Codec.Compression.GZip   as Gz

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as Ap
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map.Strict          as M
import qualified Data.Serialize           as S
import qualified Data.Text                as T
import qualified Data.Yaml                as Y
import qualified Data.Yaml.Pretty         as Yp

import           Text.Read                (readMaybe)

import           Apotheca.Bytes



data EncodingFormat
  = ShowFormat  -- Using show, read
  | BinaryFormat-- Using cereal
  | JSONFormat  -- Using aeson
  | YAMLFormat  -- Using yaml
  -- | EncryptedMode CipherStrategy SerializeMode
  deriving (Show, Read, Eq, Generic)

instance S.Serialize EncodingFormat
instance A.ToJSON EncodingFormat
instance A.FromJSON EncodingFormat
instance Encodable EncodingFormat

data GzipCompression
  = NoCompression
  | FastestCompression
  | BestCompression
  | BalancedCompression
  deriving (Show, Read, Eq, Generic)

instance S.Serialize GzipCompression
instance A.ToJSON GzipCompression
instance A.FromJSON GzipCompression
instance Encodable GzipCompression

data Encoding = Encoding
  { format :: EncodingFormat
  , gzip   :: GzipCompression
  } deriving (Show, Read, Eq, Generic)

instance S.Serialize Encoding
instance A.ToJSON Encoding
instance A.FromJSON Encoding
instance Encodable Encoding

infix 9 @>
(@>) :: EncodingFormat -> GzipCompression -> Encoding
f @> z = Encoding { format = f, gzip = z }

class (Show a, Read a, A.ToJSON a, A.FromJSON a, S.Serialize a) => Encodable a where
  encodeWithFormat :: (Encodable a) => EncodingFormat -> a -> ByteString
  encodeWithFormat ShowFormat = encodeShow
  encodeWithFormat BinaryFormat = encodeBin
  encodeWithFormat JSONFormat = encodeJSON
  encodeWithFormat YAMLFormat = encodeYAML
  decodeWithFormat :: (Encodable a) => EncodingFormat -> ByteString -> Maybe a
  decodeWithFormat ShowFormat = decodeShow
  decodeWithFormat BinaryFormat = decodeBin
  decodeWithFormat JSONFormat = decodeJSON
  decodeWithFormat YAMLFormat = decodeYAML


encodeWith :: (Encodable a) => Encoding -> a -> ByteString
encodeWith (Encoding t z) = compress z . encodeWithFormat t
decodeWith :: (Encodable a) => Encoding  -> ByteString -> Maybe a
decodeWith (Encoding t z) = decodeWithFormat t . decompress z

encodeWithFile :: (Encodable a) => FilePath -> Encoding -> a -> IO ()
encodeWithFile p t b = B.writeFile p $ encodeWith t b
decodeWithFile :: (Encodable a) => FilePath -> Encoding -> IO (Maybe a)
decodeWithFile p t = decodeWith t <$> B.readFile p

compress :: GzipCompression -> ByteString -> ByteString
compress NoCompression = id
compress z = BL.toStrict . Gz.compressWith cp . BL.fromStrict
  where
    z' = case z of
      -- NoCompression -> Gz.noCompression
      FastestCompression -> Gz.bestSpeed
      BestCompression -> Gz.bestCompression
      BalancedCompression -> Gz.defaultCompression
    cp = Gz.defaultCompressParams { Gz.compressLevel = z'}

decompress :: GzipCompression -> ByteString -> ByteString
decompress NoCompression = id
decompress _ = BL.toStrict . Gz.decompress . BL.fromStrict

-- encode :: (Show a, Read a, A.ToJSON a, S.Serialize a) =>
--   EncodingFormat -> a -> ByteString
-- encode ShowFormat = encodeShow
-- encode BinaryFormat = encodeBin
-- encode JSONFormat = encodeJSON
-- encode YAMLFormat = encodeYAML
--
-- decode :: (Show a, Read a, A.FromJSON a, S.Serialize a) =>
--   EncodingFormat -> ByteString -> Maybe a
-- decode ShowFormat = decodeShow
-- decode BinaryFormat = decodeBin
-- decode JSONFormat = decodeJSON
-- decode YAMLFormat = decodeYAML

encodeShow :: (Show a) => a -> ByteString
encodeShow = BC.pack . show
decodeShow :: (Read a) => ByteString -> Maybe a
decodeShow = readMaybe . BC.unpack

encodeBin :: (S.Serialize a) => a -> ByteString
encodeBin = S.encode
decodeBin :: (S.Serialize a) => ByteString -> Maybe a
decodeBin b = case S.decode b of
  Right a -> Just a
  Left _ -> Nothing

encodeJSON :: (A.ToJSON a) => a -> ByteString
encodeJSON = BL.toStrict . Ap.encodePretty' apconf
  where apconf = Ap.defConfig { Ap.confCompare = compare }
decodeJSON :: (A.FromJSON a) => ByteString -> Maybe a
decodeJSON = A.decode' . BL.fromStrict

encodeYAML :: (A.ToJSON a) => a -> ByteString
encodeYAML = Yp.encodePretty ypconf
  where ypconf = Yp.setConfCompare compare Yp.defConfig
decodeYAML :: (A.FromJSON a) => ByteString -> Maybe a
decodeYAML = Y.decode



-- Instances

instance A.ToJSON ByteString where
  toJSON = A.String . T.pack . b64url
instance A.FromJSON ByteString where
  parseJSON = A.withText "ByteString" $ return . unb64url . T.unpack



-- Testing

data EncodingTest = EncodingTest
  { encStrTest   :: String
  , encIntTest   :: Int
  , encFloatTest :: Float
  , encBytesTest :: ByteString
  , encListTest  :: [Int]
  , encMapTest   :: M.Map String String
  } deriving (Show, Read, Generic)

instance S.Serialize EncodingTest
instance A.ToJSON EncodingTest
instance A.FromJSON EncodingTest
instance Encodable EncodingTest

encodingTest = EncodingTest
  { encStrTest = "This is only a test!"
  , encIntTest = 15
  , encFloatTest = 5.2534
  , encBytesTest = unhex "DEADBEEF0000FFFF"
  , encListTest = [0..10]
  , encMapTest = M.fromList $
    [ ("first","first value")
    , ("second","second value")
    ]
  }

testEncodings :: [Encoding] -> IO ()
testEncodings = mapM_ (\f -> encodeWithFile (p f) f encodingTest)
  where p f = concat ["./test/",show $ format f,".",show $ gzip f,".txt"]

testAll = testEncodings encodings
  where
    fs = [ShowFormat,BinaryFormat,JSONFormat,YAMLFormat]
    zs = [NoCompression,FastestCompression,BestCompression,BalancedCompression]
    encodings = concatMap (\f -> map (f @>) zs) fs
