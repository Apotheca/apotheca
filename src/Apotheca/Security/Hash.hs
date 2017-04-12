{-# LANGUAGE DeriveGeneric #-}

module Apotheca.Security.Hash
( Hash (..)
, Salt (..)
, Digest (..)
, unsalted
, blake2
, md5
, ripemd
, skein
, sha1
, sha2
, sha3
, tiger
, whirlpool
, runHash
, HashStrategy (..)
, defaultHashStrategy
, weakHashStrategy
, hashWithStrategy
, hws
) where



import           GHC.Generics

import           Data.Bits
import           Data.ByteArray  (convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Serialize

import qualified Crypto.Hash     as H



-- TODO: Later, make a data HashSpec Family Len Trlen Salt like the prototype
-- (FAMILY)-(BITS)[tr(TBITS)] [. (...)]
--  eg: "sha2-256tr224" means sha2 w/256 bits truncated to 224
--  eg: "md5-128.sha3-512" means concat [hash md5, hash sha5-512]
--  | for xor, hashspec is split on | before .
--  So md5-128.sha1-128|sha3-512tr256 produces a 256 bit hash of md5 +
--  Alternate spec: Use : instead of tr to specify truncated, easier parsing



data Hash
  = Blake2  -- Blake2s_256
  | MD5
  | RIPEMD -- RIPEMD-160
  | SHA1
  | SHA2  -- SHA2-256
  | SHA3  -- SHA3-512
  | Skein -- Skein-512
  | Tiger
  | Whirlpool
  -- | CatHash Hash Hash
  -- | XorHash Hash Hash
  deriving (Show, Read, Eq, Generic)

type Salt = ByteString
type Digest = ByteString

unsalted :: (Salt -> ByteString -> Digest) -> ByteString -> Digest
unsalted f = f B.empty

-- Raw functions
blake2 :: Salt -> ByteString -> Digest
blake2 = ghash (undefined :: H.Blake2s_256)
md5 :: Salt -> ByteString -> Digest
md5 = ghash (undefined :: H.MD5)
ripemd :: Salt -> ByteString -> Digest
ripemd = ghash (undefined :: H.RIPEMD160)
sha1 :: Salt -> ByteString -> Digest
sha1 = ghash (undefined :: H.SHA1)
sha2 :: Salt -> ByteString -> Digest
sha2 = ghash (undefined :: H.SHA256)
sha3 :: Salt -> ByteString -> Digest
sha3 = ghash (undefined :: H.SHA3_512)
skein :: Salt -> ByteString -> Digest
skein = ghash (undefined :: H.Skein512_512)
tiger :: Salt -> ByteString -> Digest
tiger = ghash (undefined :: H.Tiger)
whirlpool :: Salt -> ByteString -> Digest
whirlpool = ghash (undefined :: H.Whirlpool)

runHash :: Hash -> Salt -> ByteString -> Digest
runHash Blake2 = blake2
runHash MD5 = md5
runHash RIPEMD = ripemd
runHash SHA1 = sha1
runHash SHA2 = sha2
runHash SHA3 = sha3
runHash Skein = skein
runHash Tiger = tiger
runHash Whirlpool = whirlpool
-- runHash _ = error "Error: Unhandled hash type!"

-- Generalized cryptonite hash helper
ghash :: (H.HashAlgorithm h) => h -> Salt -> ByteString -> Digest
ghash h s a = convert $ H.hashFinalize a'
  where
    h' = H.hashInitWith h
    s' = H.hashUpdate h' s
    a' = H.hashUpdate s' a

ghash' h = ghash h B.empty



-- Hash Strategy

-- NOTE: For real security
data HashStrategy = HashStrategy
  { algorithm      :: Hash
  , salt           :: ByteString
  , largeFileLimit :: Maybe Int -- Maximum bytes to hash before halting
  } deriving (Show, Read, Eq, Generic)

defaultHashStrategy = HashStrategy
  { algorithm = Blake2
  , salt = B.empty
  , largeFileLimit = Nothing
  }

-- NOTE: This (using a limit) is okay for identifying unique file but is not
--  sufficient for checksumming
weakHashStrategy = defaultHashStrategy
  { algorithm = RIPEMD
  , salt = B.empty
  , largeFileLimit = Just $ 2^24 -- Only hash the first 16 mb of a file for now
  }

hashWithStrategy :: HashStrategy -> ByteString -> Digest
hashWithStrategy hs bs = runHash (algorithm hs) (salt hs) bs'
  where bs' = maybe bs (`B.take` bs) (largeFileLimit hs)

-- Shorthand convenience
hws = hashWithStrategy
