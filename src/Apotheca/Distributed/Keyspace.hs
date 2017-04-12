{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Apotheca.Distributed.Keyspace
( Keyspace
, newKeyspace
, keyspaceSzBits
, keyspaceSz
, limitOf
, validate
, Key
, makeKey
, keySzBits
, keySz
, spaceOf
, valueOf
, succN
, predN
, fingerN
, allFingers
, distance
, rdistance
, hamming
, isCloser
, closer
, closest
, encodeKey
, decodeKey
-- , Keyable (..)
) where

-- Imports

import           Prelude                hiding (length)

import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Data.Data
import           Data.Word              (Word8)

import           Data.Serialize

import           Apotheca.Security.Hash


-- Data declarations


-- Keyspace

-- TODO: instead of keyspaceSz :: Int, it should be:
--  type ByteSz = Int
--  keyspaceSz :: ByteSz

newtype Keyspace = Keyspace {
  keyspaceSz :: Int -- Size of keyspace in /bytes/, not bits
} deriving (Show, Read, Eq, Bits, Data, Typeable)

newKeyspace :: Int -> Keyspace
newKeyspace = Keyspace

keyspaceSzBits :: Keyspace -> Int
keyspaceSzBits = (* 8) . keyspaceSz

limitOf :: Keyspace -> Integer
limitOf = shiftL 1 . keyspaceSzBits

validate :: Keyspace -> Key -> Bool
validate ks k = spaceOf k == ks


-- Boxed key, not exposed

newtype BoxedKey = BoxedKey {
  unbox :: Integer
} deriving (Show, Read, Eq, Bits, Data, Typeable) -- TODO: NFData, IsString


-- Exposed key

-- NOTE: This key is a bit odd. It is represents a `ByteString`, but it is
--  stored as an Integer+KeySpace internally for easy modulus arithmetic.
--  Otherwise, we'd be converting bytestrings to integers and back constantly.
--  However, this makes some differences in building a key from an integer vs
--  building a key from a bytestring, especially when considering integers or
--  bytestrings bigger than the keyspace. For this reason, the Key constructor
--  is not exposed - `makeKey` and `makeKeyBytes` should be used.

type Key = (Keyspace, BoxedKey)

makeKey :: Keyspace -> Integer -> Key
makeKey ks i = (ks, BoxedKey $ i `mod` limitOf ks)

makeKeyBytes :: Keyspace -> ByteString -> Key
makeKeyBytes ks bs = makeKey ks (b2i bs)

keySzBits :: Key -> Int
keySzBits = keyspaceSzBits . spaceOf

keySz :: Key -> Int
keySz = keyspaceSz . spaceOf

spaceOf :: Key -> Keyspace
spaceOf = fst

sharesSpace :: Key -> Key -> Bool
sharesSpace = validate . spaceOf
-- Equally valid definitions
-- sharesSpace k k2 = validate (spaceOf k) k2
-- sharesSpace k k2 = spaceOf k == spaceOf k2

valueOf :: Key -> Integer
valueOf = unbox . snd

succN :: Integer -> Key -> Key
succN i k = makeKey (spaceOf k) $ i + valueOf k

predN :: Integer -> Key -> Key
predN i = succN (-i)

-- NOTE: i should be < keyspaceBits
fingerN :: Int -> Key -> Key
fingerN i k
    | i == 0 = k
    | otherwise = relN i' k
  where
    relN = if i > 0 then succN else predN
    i' = 2 ^ ((abs i) - 1)

allFingers :: Key -> [Key]
allFingers k = [fingerN n k | n <- [0..(keySzBits k)]]

-- Distance of k from origin a
-- NOTE: Currently distance does not ensure keys share a space
distance :: Key -> Key -> Integer
distance a k = (valueOf k - valueOf a) `mod` (limitOf . spaceOf) a

--  distance a k + rdistance k a == limit k
rdistance :: Key -> Key -> Integer
rdistance a k = distance k a

hamming :: Key -> Int
hamming = popCount . valueOf

-- returns true if a is closer to k than b, order relative to k
isCloser :: Key -> Key -> Key -> Bool
isCloser k a b = distance k a < distance k b

-- returns a unless b is closer to k than a
closer :: Key -> Key -> Key -> Key
closer k a b = if isCloser k b a then b else a

closest :: Key -> [Key] -> Key
closest _ [] = error "Cannot be called on empty list."
closest k ks = foldl1 (closer k) ks



-- Conversion

-- TODO: Conversion to and from different keyspaces is potentially lossy
--  Eg, converting down to a smaller keyspace, and back up, drops bytes
--    0xDEADBEEF1234 -> 0xDEADBEEF -> 0xDEADBEEF0000
--  For this reason, keys should only be upconverted.

-- NOTE: Converting directly from a bytestring to a key does not necessarily
--  produce the same key as converting the canonical binary integer value of
--  that string to a key. This is because converting an int via `makeKey`
--  requires an explicit length, and the int has the most-significant digits
--  truncated via `modulus`, whereas (fromBytes . forceLen) truncates tailing
--  bytes.
--  For these reasons, `makeKeyBytes` is preferred over fromBytes due to
--  explicit length and the bytestring being converted to an integer first,
--  otherwise it is technically possible to assemble a key with too large of a
--  value.

b2i :: ByteString -> Integer
b2i bs = B.foldl (\i w -> shiftL i 8 .|. fromIntegral w) 0 bs

i2b :: Int -> Integer -> ByteString
i2b n i = fst $ B.unfoldrN n f (n - 1)
  where
    f :: Int -> Maybe (Word8, Int)
    f n = Just (fromIntegral $ shiftR i (8 * n) .&. 255, n - 1)

decodeKey :: ByteString -> Key
decodeKey bs = (newKeyspace $ B.length bs, BoxedKey $ b2i bs)

encodeKey :: Key -> ByteString
encodeKey k = i2b (keySz k) (valueOf k)

-- instance Serialize Key where
--   put k = put . encodeKey
--   get = decodeKey <$> (get :: Get ByteString)

-- class Keyable a where
--   defaultKeyGen :: a -> ByteString
--   -- defaultKeyGen = unsalted sha2
--   genKey :: a -> Key
--   genKey = genKeyWith defaultKeyGen
--   genKeyWith :: (a -> ByteString) -> a -> Key
--   genKeyWith f a = fromBytes $ f a
