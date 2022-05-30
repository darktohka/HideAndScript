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
    ( HMACResult, hmacLength, createHMACKey, calculateHMAC )
import ImgCrypto.XSalsa
    ( XSalsaNonce,
      xSalsaNonceLength,
      createXSalsa20Key,
      encryptXSalsa20 )

-- Data types

-- Each file header contains the length of the cryptstring,
-- the XSalsa20 nonce, the HMAC digest and the cryptstring itself
data FileHeader = FileHeader {
    length :: !GH.Int64,
    nonceContent :: !XSalsaNonce,
    hmacContent :: !HMACResult,
    remainingContent :: !BL.ByteString
} deriving (Eq, Show)

-- Encryption

-- Encrypts an unencrypted ByteString into a XSalsa20 crypttext.
-- The result of this function is a well-defined FileHeader that
-- can be saved into an image.
encryptFile :: CV.SecretKey -> XSalsaNonce -> BL.ByteString -> BS.ByteString
encryptFile curveKey nonce finalData = finalHeader
    where
        -- First, compress the data
        compressedLazyData = Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression } finalData
        compressedData = BS.toStrict compressedLazyData

        -- Second, generate the XSalsa20 key using HKDF SHA256,
        -- then encrypt the compressed data
        salsaKey = createXSalsa20Key nonce curveKey
        salsaContent = encryptXSalsa20 salsaKey nonce compressedData

        -- Third, create an HMAC key from hashing the salsa key and nonce
        -- together with SHA256, then HMAC the crypttext
        hmacKey = createHMACKey salsaKey nonce
        hmacContent = calculateHMAC hmacKey salsaContent

        -- Fourth, store the length of the crypttext
        salsaLength = BS.length salsaContent
        salsaLengthEncoded = BL.toStrict (BSB.toLazyByteString (BSB.word32BE (fromIntegral salsaLength)))

        -- Finally, build the final header
        finalHeader = BS.concat [salsaLengthEncoded, nonce, hmacContent, salsaContent]

-- Decryption

-- Reads the file header into a BG.Get instance.
getFileHeader :: BG.Get FileHeader
getFileHeader = do
    length <- BG.getWord32be
    nonceContent <- BG.getByteString xSalsaNonceLength
    hmacContent <- BG.getByteString hmacLength
    remainingContent <- BG.getRemainingLazyByteString
    return $! FileHeader (fromIntegral length) nonceContent hmacContent remainingContent

-- Decodes a ByteString into a file header.
decodeFileHeader :: BL.ByteString -> FileHeader
decodeFileHeader fileContent = BG.runGet getFileHeader fileContent

-- Decrypts an encrypted XSalsa20 crypttext into an unencrypted ByteString.
-- This function can only be used on crypttexts containing the well-defined FileHeader!
-- The result of this function is a well-defined FileHeader that can be saved into an image.
decryptFile :: CV.SecretKey -> BL.ByteString -> Maybe BL.ByteString
decryptFile curveKey finalHeader = finalResult
    where
        -- First, extract the data from the header.
        (FileHeader length nonceContent hmacContent remainingContent) = decodeFileHeader finalHeader
        remainingContentLength = BL.length remainingContent

        -- Second, extract the XSalsa20 crypttext and rederive the key using the nonce in the header.
        salsaContent = BL.toStrict $ BL.take (fromIntegral length) remainingContent
        salsaKey = createXSalsa20Key nonceContent curveKey

        -- Third, rederive the HMAC key using the XSalsa20 key and the nonce.
        hmacKey = createHMACKey salsaKey nonceContent
        currentHmac = calculateHMAC hmacKey salsaContent

        -- Fourth, decrypt the XSalsa20 crypttext and decompress it.
        decryptedContent = encryptXSalsa20 salsaKey nonceContent salsaContent
        decompressedContent = Z.decompress $ BL.fromStrict decryptedContent

        -- There are two ways we can fail if this is an invalid file:
        -- 1. The image contains less data than we've promised in the header.
        -- 2. The HMAC code does not match with the XSalsa20 crypttext.
        finalResult = case remainingContentLength >= length && currentHmac == hmacContent of
            True -> Just decompressedContent
            False -> Nothing
