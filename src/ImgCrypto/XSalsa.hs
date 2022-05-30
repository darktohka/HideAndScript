module ImgCrypto.XSalsa (
    XSalsaNonce,
    XSalsaKey,
    xSalsaNonceLength,
    xSalsaRounds,
    createXSalsa20Nonce,
    createXSalsa20Key,
    encryptXSalsa20
) where

import qualified Crypto.PubKey.Curve25519 as CV
import qualified Crypto.KDF.HKDF as HK
import qualified Crypto.Cipher.XSalsa as XS
import qualified Crypto.Hash as CH
import qualified Crypto.Random as CR
import qualified Data.ByteString as BS

-- Data types

-- An XSalsa20 nonce value.
type XSalsaNonce = BS.ByteString

-- An XSalsa20 key.
type XSalsaKey = HK.PRK CH.SHA256

-- Constants

-- Returns the length of an XSalsa20 nonce.
xSalsaNonceLength :: Int
xSalsaNonceLength = 24

-- Returns the amount of times the XSalsa20 algorithm is ran.
xSalsaRounds :: Int
xSalsaRounds = 20

-- Functions

-- Creates an XSalsa20 nonce value.
-- This relies on the system's source of randomness.
-- As such, it can only be run inside an IO Monad.
createXSalsa20Nonce :: IO XSalsaNonce
createXSalsa20Nonce = CR.getRandomBytes xSalsaNonceLength

-- Creates an XSalsa20 key from a nonce and a Curve25519 secret key.
-- This is made possible by the HKDF algorithm used.
createXSalsa20Key :: XSalsaNonce -> CV.SecretKey -> XSalsaKey
createXSalsa20Key nonce key = HK.extract nonce key

-- Encrypts a ByteString using a XSalsa20 key and nonce.
-- WARNING! Nonces must never be reused!
encryptXSalsa20 :: XSalsaKey -> XSalsaNonce -> BS.ByteString -> BS.ByteString
encryptXSalsa20 key nonce finalData = fst $ XS.combine initialState finalData
    where
        initialState = XS.initialize xSalsaRounds key nonce
