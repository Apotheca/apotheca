{-# LANGUAGE DeriveGeneric #-}

module Apotheca.Security.Cipher
( Cipher (..)
, cipherNonceSize
, cipherKeySize
, runCipher
, encipherWith
, decipherWith
, CipherStrategy (..)
, defaultCipherStrategy
-- Derive keys
, derive
, deriveWith
-- Header
, CipherHeader (..)
, encipherHeaderWith
, decipherHeaderWith
-- Nonce
, makeNonce
, getNonceIO
) where

import           GHC.Generics

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.Maybe

import qualified Crypto.Cipher.AES      as AES
import qualified Crypto.Cipher.Blowfish as Blowfish
import qualified Crypto.Cipher.ChaCha   as Cha
import qualified Crypto.Cipher.Salsa    as Salsa
import           Crypto.Cipher.Types    (BlockCipher, cipherInit, ctrCombine)
import qualified Crypto.Cipher.Types    as C
import           Crypto.Error
import qualified Crypto.Hash.Algorithms as H
import qualified Crypto.KDF.PBKDF2      as KDF

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Security.Hash

-- https://en.wikipedia.org/wiki/Cipher_suite

data Cipher
  = AES256
  | Blowfish448
  | ChaCha256
  | Salsa256
  -- | NoCipher
  deriving (Show, Read, Eq, Generic)

instance Serialize Cipher
instance ToJSON Cipher
instance FromJSON Cipher
instance Encodable Cipher

cipherNonceSize :: Cipher -> Int
cipherNonceSize AES256 = 16
cipherNonceSize Blowfish448 = 8
cipherNonceSize ChaCha256 = 12
cipherNonceSize Salsa256 = 12

cipherKeySize :: Cipher -> Int
cipherKeySize AES256 = 32
cipherKeySize Blowfish448 = 56
cipherKeySize ChaCha256 = 32
cipherKeySize Salsa256 = 32

runCipher :: Cipher -> Nonce -> ByteString -> ByteString -> Failable ByteString
runCipher AES256 n k pt = do
  validateCipher AES256 n k
  gcipher (undefined :: AES.AES256) n k pt
runCipher Blowfish448 n k pt = do
  validateCipher Blowfish448 n k
  gcipher (undefined :: Blowfish.Blowfish448) n k pt
runCipher ChaCha256 n k pt = do
  validateCipher ChaCha256 n k
  return . fst $ Cha.combine (Cha.initialize 20 k n) pt
runCipher Salsa256 n k pt = do
  validateCipher Salsa256 n k
  return . fst $ Salsa.combine (Salsa.initialize 20 k n) pt

validateCipher :: Cipher -> Nonce -> ByteString -> Failable ()
validateCipher c n k = cf2f $ case (B.length n == cnsz, B.length k == cksz) of
    (True, True) -> CryptoPassed ()
    (False, _) -> CryptoFailed CryptoError_IvSizeInvalid
    (_,False) -> CryptoFailed CryptoError_KeySizeInvalid
  where
    cnsz = cipherNonceSize c
    cksz = cipherKeySize c

encipherWith :: CipherStrategy -> Nonce -> Secret -> ByteString -> Failable ByteString
encipherWith cs n k pt = runCipher c n k pt
  where
    c = calgorithm cs
    k' = if deriveKey cs
      then derive (cipherKeySize c) n k
      else k

decipherWith = encipherWith

type Nonce = ByteString
type Secret = ByteString

data CipherStrategy = CipherStrategy
  { calgorithm :: Cipher
  , deriveKey  :: Bool -- TODO: Make Maybe KDFStrategy
  , preHasher  :: Maybe HashStrategy
  , postHasher :: Maybe HashStrategy
  }
  deriving (Show, Read, Eq, Generic)

instance Serialize CipherStrategy
instance ToJSON CipherStrategy
instance FromJSON CipherStrategy
instance Encodable CipherStrategy

-- data CipherHeader = CipherHeader
--   { cipherStrategy :: CipherStrategy
--   }

defaultCipherStrategy = CipherStrategy
  { calgorithm = AES256
  , deriveKey = True
  , preHasher = Nothing
  , postHasher = Nothing
  }

-- Simple derive for now: length, salt/nonce, bytes
derive :: Int -> Nonce -> ByteString -> ByteString
derive l s k = KDF.generate prf params k s
  where
    prf = KDF.prfHMAC H.SHA256 :: KDF.PRF ByteString
    params = KDF.Parameters 2048 l

deriveWith :: CipherStrategy -> Nonce -> ByteString -> ByteString
deriveWith cs s k = k'
  where
    k' = if deriveKey cs
      then derive (cipherKeySize . calgorithm $ cs) s k
      else k

--

-- NOTE: Nonce and key must be validated before this is called
gcipher :: C.BlockCipher c => c -> Nonce -> ByteString -> ByteString -> Failable ByteString
gcipher c iv key pt = cf2f $ do
    ctx <- cipherInit key
    iv' <- giv c iv
    return $ ctrCombine ctx iv' pt
  where
    giv :: C.BlockCipher c => c -> Nonce -> CryptoFailable (C.IV c)
    giv c iv = case C.makeIV iv of
      Just iv' -> CryptoPassed iv'
      Nothing -> CryptoFailed CryptoError_IvSizeInvalid




-- Cipher header

data CipherHeader = CipherHeader
  { cipherStrategy :: CipherStrategy
  , nonce          :: Nonce
  , preHash        :: Maybe Digest -- Could be HashHeader, but would be duplicate data re CipherStrategy
  , postHash       :: Maybe Digest
  } deriving (Show, Read, Eq, Generic)

instance Serialize CipherHeader
instance ToJSON CipherHeader
instance FromJSON CipherHeader
instance Encodable CipherHeader

encipherHeaderWith :: CipherStrategy -> Nonce -> Secret -> ByteString -> Failable (CipherHeader, ByteString)
encipherHeaderWith cs n k pt = do
    ct <- encipherWith cs n k' pt
    return (makeHeader ct, ct)
  where
    hf x = Just . (`hashWith` x)
    k' = deriveWith cs n k
    makeHeader ct = CipherHeader
      { cipherStrategy = cs
      , nonce = n
      , preHash = maybe Nothing (hf pt) (preHasher cs)
      , postHash = maybe Nothing (hf ct) (postHasher cs)
      }

decipherHeaderWith :: CipherHeader -> Secret -> ByteString -> Failable ByteString
decipherHeaderWith ch k ct = do
    -- CryptoError_MacKeyInvalid
    -- Post-hash / cryptext check
    case postHasher cs of
      Just h | Just (hashWith h ct) /= postHash ch ->
        failed $ show CryptoError_MacKeyInvalid
      _ -> passed ()
    -- Decrypt attempt
    pt <- decipherWith cs n k' ct
    -- Prehash /plaintext check
    case preHasher cs of
      Just h | Just (hashWith h pt) /= preHash ch ->
        failed $ show CryptoError_MacKeyInvalid
      _ -> passed ()
    return pt
  where
    hf x = Just . (`hashWith` x)
    cs = cipherStrategy ch
    n = nonce ch
    k' = deriveWith cs n k


-- Simple either convert
type Failable a = Either String a

cf2f :: CryptoFailable a -> Failable a
cf2f (CryptoPassed a) = passed a
cf2f (CryptoFailed e) = failed $ show e

passed :: a -> Failable a
passed = Right
failed :: String -> Failable a
failed = Left



-- Nonce

makeNonce :: Cipher -> ByteString -> Nonce
makeNonce c n = forceLen (cipherNonceSize c) n

getNonceIO :: Cipher -> IO Nonce
getNonceIO c = getRandomBytesIO (cipherNonceSize c)
