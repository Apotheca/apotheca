{-# LANGUAGE DeriveGeneric #-}

module Apotheca.Security.Auth
( Auth (..)
, checkAuth
, decodeAuth
, forceDecodeAuth
, generatePassthru
, generateDerived
, generatePayload
, readAuth
, writeAuth
, withAuth
, withAuthFile
) where

import           GHC.Generics

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash

-- Auth is a simplistic hash auth to determine validity of a password / key, but
--  instead of returning a bool, it returns a Maybe ByteString that is one of:
--  1) The password itself - this is auth at its simplest True / False usage
--  2) A transformed / derived key -
--  3) An encrypted payload transformed back into plaintext via the password
-- This allows us to segregate authorization from encryption, and apply otherwise
--  difficult behaviors, such as changing a password higher in the hierarchy without
--  having to re-encrypt everything beneath.
data Auth = Auth
  { authHashHeader    :: HashHeader
  , authCipherPayload :: Maybe (CipherHeader, ByteString)
  , authNonce         :: ByteString -- Derivation nonce
  , authDidDerive     :: Bool
  } deriving (Show, Read, Eq, Generic)

instance Serialize Auth
instance ToJSON Auth
instance FromJSON Auth
instance Encodable Auth



-- Generating auths

initAuth hs p = Auth
  { authHashHeader = hashHeaderWith hs p
  , authCipherPayload = Nothing
  , authNonce = B.empty
  , authDidDerive = False
  }

generatePassthru :: HashStrategy -> ByteString -> Auth
generatePassthru hs p = initAuth hs p

generateDerived :: HashStrategy -> ByteString -> ByteString -> Auth
generateDerived hs n p = (initAuth hs p)
  { authNonce = n
  , authDidDerive = True
  }

generatePayload :: HashStrategy -> CipherStrategy -> ByteString -> ByteString -> ByteString -> Auth
generatePayload hs cs n p pld = (initAuth hs p)
    { authCipherPayload = Just (ch, ct)
    }
  where
    Right (ch, ct) = encipherHeaderWith cs (makeNonce (calgorithm cs) n) p pld



-- Derived keys
-- TODO: Use DeriveStrategy when implemented

deriveAuth :: ByteString -> ByteString -> ByteString
deriveAuth n p = derive 32 n p

deriveIf :: Bool -> ByteString -> ByteString -> ByteString
deriveIf b n p = if b then deriveAuth n p else p



-- Check and decode

checkAuth :: Auth -> ByteString -> Bool
checkAuth au p = validateHashHeader (authHashHeader au) p

decodeAuth :: Auth -> ByteString -> Maybe ByteString
decodeAuth au p = if checkAuth au p
    then case authCipherPayload au of
      Just (ch, ct) -> either (const Nothing) Just $ decipherHeaderWith ch p' ct
      Nothing -> Just p'
    else Nothing
  where
    p' = deriveIf (authDidDerive au) (authNonce au) p

forceDecodeAuth :: Auth -> ByteString -> ByteString
forceDecodeAuth au p = fromJust $ decodeAuth au p



-- File IO

authEncoding = JSONFormat @> NoCompression

readAuth :: FilePath -> IO Auth
readAuth p = do
  mau <- decodeWithFile p authEncoding
  case mau of
    Just au -> return au
    Nothing -> error "Could not parse auth."

writeAuth :: FilePath -> Auth -> IO ()
writeAuth p = encodeWithFile p authEncoding



-- Helpers

withAuth :: Auth -> ByteString -> (ByteString -> IO a) -> IO a
withAuth au p f = case decodeAuth au p of
  Just bs -> f bs
  Nothing -> error "Could not decode auth."

withAuthFile :: FilePath -> ByteString -> (ByteString -> IO a) -> IO a
withAuthFile fp p f = do
  au <- readAuth fp
  withAuth au p f
