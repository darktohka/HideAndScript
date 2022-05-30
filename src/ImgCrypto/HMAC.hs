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

import ImgCrypto.XSalsa

type HMACKey = CH.Digest CH.SHA256
type HMACDigest = CH.Digest CH.SHA256
type HMACResult = BS.ByteString

hmacLength :: Int
hmacLength = 32

createHMACKey :: XSalsaKey -> XSalsaNonce -> HMACKey
createHMACKey salsaKey salsaNonce = finalizedHash
    where
        initialHash = CH.hashInit :: CH.Context CH.SHA256
        keyedHash = CH.hashUpdate initialHash salsaKey
        noncedHash = CH.hashUpdate keyedHash salsaNonce
        finalizedHash = CH.hashFinalize noncedHash

calculateHMACDigest :: HMACKey -> BS.ByteString -> HMACDigest
calculateHMACDigest key message = HMAC.hmacGetDigest $ HMAC.hmac key message

calculateHMAC :: HMACKey -> BS.ByteString -> HMACResult
calculateHMAC key message = U.byteArrayToByteString $ calculateHMACDigest key message
