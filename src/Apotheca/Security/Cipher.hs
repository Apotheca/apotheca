{-# LANGUAGE DeriveGeneric #-}

module Apotheca.Security.Cipher
( Cipher (..)
, cipherNonceSize
, cipherKeySize
, runCipher
, CipherStrategy (..)
, defaultCipherStrategy
, derive
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

import           Apotheca.Encodable

-- https://en.wikipedia.org/wiki/Cipher_suite

data Cipher
  = AES256
  | Blowfish448
  | ChaCha256
  | Salsa256
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

runCipher :: Cipher -> Nonce -> ByteString -> ByteString -> CryptoFailable ByteString
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

validateCipher :: Cipher -> Nonce -> ByteString -> CryptoFailable ()
validateCipher c n k = case (B.length n == cnsz, B.length k == cksz) of
    (True, True) -> CryptoPassed ()
    (False, _) -> CryptoFailed CryptoError_IvSizeInvalid
    (_,False) -> CryptoFailed CryptoError_KeySizeInvalid
  where
    cnsz = cipherNonceSize c
    cksz = cipherKeySize c

type Nonce = ByteString

data CipherStrategy = CipherStrategy
  { algorithm      :: Cipher
  , deriveValidKey :: Bool
  }
  deriving (Show, Read, Eq, Generic)

instance Serialize CipherStrategy
instance ToJSON CipherStrategy
instance FromJSON CipherStrategy
instance Encodable CipherStrategy

defaultCipherStrategy = CipherStrategy
  { algorithm = AES256
  , deriveValidKey = False
  }

derive :: Int -> Nonce -> ByteString -> ByteString
derive l s k = KDF.generate prf params k s
  where
    prf = KDF.prfHMAC H.SHA256 :: KDF.PRF ByteString
    params = KDF.Parameters 2048 l

--

-- NOTE: Nonce and key must be validated before this is called
gcipher :: C.BlockCipher c => c -> Nonce -> ByteString -> ByteString -> CryptoFailable ByteString
gcipher c iv key pt =  do
    ctx <- cipherInit key
    iv' <- giv c iv
    return $ ctrCombine ctx iv' pt
  where
    giv :: C.BlockCipher c => c -> Nonce -> CryptoFailable (C.IV c)
    giv c iv = case C.makeIV iv of
      Just iv' -> CryptoPassed iv'
      Nothing -> CryptoFailed CryptoError_IvSizeInvalid
