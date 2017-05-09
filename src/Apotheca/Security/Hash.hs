{-# LANGUAGE DeriveGeneric #-}

module Apotheca.Security.Hash
( Hash (..)
, Salt (..)
, Digest (..)
, availableHashes
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
, newHashStrategy
, newHashStrategy'
, newHashStrategy''
, hashWith
, hws
--
, HashHeader (..)
, hashHeaderWith
, validateHashHeader
) where



import           GHC.Generics

import           Data.Bits
import           Data.ByteArray     (convert)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as B
import           Data.Serialize

import qualified Crypto.Hash        as H

import           Apotheca.Encodable



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
  deriving (Show, Read, Eq, Enum, Generic)

instance Serialize Hash
instance ToJSON Hash
instance FromJSON Hash
instance Encodable Hash

type Salt = ByteString
type Digest = ByteString

availableHashes :: [Hash]
availableHashes = enumFrom Blake2

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

-- Generalized cryptonite hash helper - easier than ciphers
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
  { halgorithm :: Hash
  , salt       :: ByteString
  , hlimit     :: Maybe Int -- Maximum bytes to hash before halting
  } deriving (Show, Read, Eq, Generic)

instance Serialize HashStrategy
instance ToJSON HashStrategy
instance FromJSON HashStrategy
instance Encodable HashStrategy

defaultHashStrategy = HashStrategy
  { halgorithm = Blake2
  , salt = B.empty
  , hlimit = Nothing
  }

newHashStrategy :: Hash -> HashStrategy
newHashStrategy h = newHashStrategy' h B.empty

newHashStrategy' :: Hash -> Salt -> HashStrategy
newHashStrategy' h s = newHashStrategy'' h s Nothing

newHashStrategy'' :: Hash -> ByteString -> Maybe Int -> HashStrategy
newHashStrategy'' h s l = defaultHashStrategy
  { halgorithm = h
  , salt = s
  , hlimit = l
  }

hashWith :: HashStrategy -> ByteString -> Digest
hashWith hs bs = runHash (halgorithm hs) (salt hs) bs'
  where bs' = maybe bs (`B.take` bs) (hlimit hs)

-- Shorthand convenience
hws = hashWith



-- Header

data HashHeader = HashHeader
  { hashStrategy :: HashStrategy
  , hashValue    :: Digest
  } deriving (Show, Read, Eq, Generic)

instance Serialize HashHeader
instance ToJSON HashHeader
instance FromJSON HashHeader
instance Encodable HashHeader

hashHeaderWith :: HashStrategy -> ByteString -> HashHeader
hashHeaderWith hs bs = HashHeader
  { hashStrategy = hs
  , hashValue = hashWith hs bs
  }

validateHashHeader :: HashHeader -> ByteString -> Bool
validateHashHeader hh bs = hashWith (hashStrategy hh) bs == hashValue hh
