module Caligo.Bytes
( hex, unhex
, b64, unb64
, b64url, unb64url
, forceLen
, chunksOf
, readableBytesizeSI
, readableBytesizeIEC
, getRandomBytesIO
, ByteString (..)
) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as BC
import qualified Data.List                  as L

import qualified Crypto.Random              as Rnd

import           Text.Printf                (printf)

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), defaultOptions,
                                             foldable, genericToEncoding,
                                             object, pairs, (.:), (.=))



-- Various encodings

hex :: ByteString -> String
hex = BC.unpack . B16.encode

unhex :: String -> ByteString
unhex = (fst . B16.decode) . BC.pack

b64 :: ByteString -> String
b64 = BC.unpack . B64.encode

-- NOTE: UNSAFE - assumes bytestring is in good format
unb64 :: String -> ByteString
unb64 s = b
  where Right b = B64.decode . BC.pack $ s

b64url :: ByteString -> String
b64url = BC.unpack . B64U.encode

-- NOTE: UNSAFE - assumes bytestring is in good format
unb64url :: String -> ByteString
unb64url s = b
  where Right b = B64U.decode . BC.pack $ s



-- Bytestring utilities

forceLen :: Int -> ByteString -> ByteString
forceLen i bs
    | i == bl = bs
    | i < bl = B.take i bs -- Truncate
    | i > bl = B.append bs $ B.replicate (i - bl) 0 -- Zero-fill
  where bl = B.length bs

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf i = L.unfoldr $ \b -> if B.null b
  then Nothing
  else Just $ B.splitAt i b



-- Human-readable bytesize strings

readableSize :: Int -> [String] -> Int -> String
readableSize base suffixes n = printf "%.1f%s" n' $ suffixes !! pow
  where
    -- Lots of fromIntegral here
    base' = fromIntegral base
    pow = floor . logBase base' $ fromIntegral n
    n' = (fromIntegral n :: Float) / fromIntegral (base ^ pow)

readableBytesizeSI :: Int -> String
readableBytesizeSI = readableSize 1000 suffixes
  where suffixes = [ "B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB" ]

readableBytesizeIEC :: Int -> String
readableBytesizeIEC = readableSize 1024 suffixes
  where suffixes = [ "B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB" ]

getRandomBytesIO :: Int -> IO ByteString
getRandomBytesIO i = fst . Rnd.randomBytesGenerate i <$> Rnd.drgNew
