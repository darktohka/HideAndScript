module ImgCrypto.HMAC (
    HMACKey,
    HMACDigest,
    HMACResult,
    hmacLength,
    createHMACKey,
    calculateHMACDigest,
    calculateHMAC
) where

import qualified Crypto.MAC.HMAC as HMAC
import qualified Crypto.Hash as CH
import qualified Data.ByteString as BS
import qualified Utils as U

import ImgCrypto.XSalsa ( XSalsaKey, XSalsaNonce )

-- Types

-- An HMAC key.
type HMACKey = CH.Digest CH.SHA256

-- An HMAC digest.
type HMACDigest = CH.Digest CH.SHA256

-- An HMAC digest, represented as a ByteString.
type HMACResult = BS.ByteString

-- Constants

-- Returns the length of an HMAC digest.
hmacLength :: Int
hmacLength = 32

-- Functions

-- Creates an HMAC key from the XSalsa20 key and nonce.
-- This way, the HMAC authenticates the crypt text, the nonce AND the key at the same time.
-- The HMAC key is created using a simple SHA256 hash concatenation.
createHMACKey :: XSalsaKey -> XSalsaNonce -> HMACKey
createHMACKey salsaKey salsaNonce = finalizedHash
    where
        initialHash = CH.hashInit :: CH.Context CH.SHA256
        keyedHash = CH.hashUpdate initialHash salsaKey
        noncedHash = CH.hashUpdate keyedHash salsaNonce
        finalizedHash = CH.hashFinalize noncedHash

-- Calculates the HMAC digest of a ByteString given an HMACKey.
calculateHMACDigest :: HMACKey -> BS.ByteString -> HMACDigest
calculateHMACDigest key message = HMAC.hmacGetDigest $ HMAC.hmac key message

-- Calculates the HMAC digest of a ByteString given an HMACKey as a ByteString.
calculateHMAC :: HMACKey -> BS.ByteString -> HMACResult
calculateHMAC key message = U.byteArrayToByteString $ calculateHMACDigest key message
