module ImgCrypto.Curve25519 (
    createCurve25519Key,
    loadCurve25519Key,
    getCurve25519Key
) where

import qualified Crypto.PubKey.Curve25519 as CV
import qualified Crypto.Error as CE
import qualified Data.ByteString as BS
import qualified System.Directory as SD
import qualified Utils as U

-- Returns the secret filename containing our Curve25519 key.
secretFilename :: FilePath
secretFilename = "curve25519.secret"

-- Attempts to create a Curve25519 key.
-- As soon as the key is created, it is saved into a file.
createCurve25519Key :: FilePath -> IO (Maybe CV.SecretKey)
createCurve25519Key filename = do
    key <- CV.generateSecretKey

    -- Save the Curve25519 key into a file.
    BS.writeFile filename $ U.byteArrayToByteString key

    return $ Just key

-- Attempts to load a Curve25519 key.
-- This can fail, for example if the key file contains less than 32 bytes.
-- In that case, a Nothing monad is returned.
loadCurve25519Key :: FilePath -> IO (Maybe CV.SecretKey)
loadCurve25519Key filename = do
    file <- BS.readFile filename

    case CV.secretKey file of
        CE.CryptoFailed _ -> return Nothing
        CE.CryptoPassed key -> return $ Just key

-- Gets our Curve25519 key no matter what.
-- If the key does not exist, it is created.
-- If the key exists, it is loaded.
-- This can fail. In that case, a Nothing monad is returned.
getCurve25519Key :: IO (Maybe CV.SecretKey)
getCurve25519Key = do
    exists <- SD.doesFileExist secretFilename

    case exists of
        True -> loadCurve25519Key secretFilename
        _ -> createCurve25519Key secretFilename
