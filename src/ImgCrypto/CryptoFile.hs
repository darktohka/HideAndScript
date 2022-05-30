module ImgCrypto.CryptoFile (
    FileHeader,
    encryptFile,
    decryptFile
) where

import qualified Crypto.PubKey.Curve25519 as CV
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as BS
import qualified Data.Binary.Get as BG
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import qualified GHC.Int as GH

import ImgCrypto.HMAC
import ImgCrypto.XSalsa

-- Data types
data FileHeader = FileHeader {
    length :: !GH.Int64,
    nonceContent :: !XSalsaNonce,
    hmacContent :: !HMACResult,
    remainingContent :: !BL.ByteString
} deriving (Eq, Show)

-- Encryption

encryptFile :: CV.SecretKey -> XSalsaNonce -> BL.ByteString -> BS.ByteString
encryptFile curveKey nonce finalData = finalHeader
    where
        compressedLazyData = Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression } finalData
        compressedData = BS.toStrict compressedLazyData

        salsaKey = createXSalsa20Key nonce curveKey
        salsaContent = encryptXSalsa20 salsaKey nonce compressedData

        hmacKey = createHMACKey salsaKey nonce
        hmacContent = calculateHMAC hmacKey salsaContent

        salsaLength = BS.length salsaContent
        salsaLengthEncoded = BL.toStrict (BSB.toLazyByteString (BSB.word32BE (fromIntegral salsaLength)))

        finalHeader = BS.concat [salsaLengthEncoded, nonce, hmacContent, salsaContent]

-- Decryption

getFileHeader :: BG.Get FileHeader
getFileHeader = do
    length <- BG.getWord32be
    nonceContent <- BG.getByteString xSalsaNonceLength
    hmacContent <- BG.getByteString hmacLength
    remainingContent <- BG.getRemainingLazyByteString
    return $! FileHeader (fromIntegral length) nonceContent hmacContent remainingContent

decodeFileHeader :: BL.ByteString -> FileHeader
decodeFileHeader fileContent = BG.runGet getFileHeader fileContent

decryptFile :: CV.SecretKey -> BL.ByteString -> Maybe BL.ByteString
decryptFile curveKey finalHeader = finalResult
    where
        (FileHeader length nonceContent hmacContent remainingContent) = decodeFileHeader finalHeader
        remainingContentLength = BL.length remainingContent

        salsaContent = BL.toStrict $ BL.take (fromIntegral length) remainingContent
        salsaKey = createXSalsa20Key nonceContent curveKey

        hmacKey = createHMACKey salsaKey nonceContent
        currentHmac = calculateHMAC hmacKey salsaContent

        decryptedContent = encryptXSalsa20 salsaKey nonceContent salsaContent
        decompressedContent = Z.decompress $ BL.fromStrict decryptedContent

        finalResult = case remainingContentLength >= length && currentHmac == hmacContent of
            True -> Just decompressedContent
            False -> Nothing
