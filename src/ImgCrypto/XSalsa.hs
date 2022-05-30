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

type XSalsaNonce = BS.ByteString
type XSalsaKey = HK.PRK CH.SHA256

xSalsaNonceLength = 24
xSalsaRounds = 20

createXSalsa20Nonce :: IO XSalsaNonce
createXSalsa20Nonce = CR.getRandomBytes xSalsaNonceLength

createXSalsa20Key :: XSalsaNonce -> CV.SecretKey -> XSalsaKey
createXSalsa20Key nonce key = HK.extract nonce key

encryptXSalsa20 :: XSalsaKey -> XSalsaNonce -> BS.ByteString -> BS.ByteString
encryptXSalsa20 key nonce finalData = fst $ XS.combine initialState finalData
    where
        initialState = XS.initialize xSalsaRounds key nonce
