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

secretFilename :: FilePath
secretFilename = "curve25519.secret"

createCurve25519Key :: FilePath -> IO (Maybe CV.SecretKey)
createCurve25519Key filename = do
    key <- CV.generateSecretKey

    BS.writeFile filename $ U.byteArrayToByteString key

    return $ Just key

loadCurve25519Key :: FilePath -> IO (Maybe CV.SecretKey)
loadCurve25519Key filename = do
    file <- BS.readFile filename

    case CV.secretKey file of
        CE.CryptoFailed _ -> return Nothing
        CE.CryptoPassed key -> return $ Just key

getCurve25519Key :: IO (Maybe CV.SecretKey)
getCurve25519Key = do
    exists <- SD.doesFileExist secretFilename

    case exists of
        True -> loadCurve25519Key secretFilename
        _ -> createCurve25519Key secretFilename
