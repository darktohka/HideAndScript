module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Web.Browser as WB
import qualified System.FilePath as FP
import qualified System.Directory as SD

import ImgCrypto.Curve25519
import ImgCrypto.XSalsa
import ImgCrypto.CryptoFile

import GUI.ImagePrompt
import GUI.FolderPrompt
import GUI.PythonPrompt
import GUI.MessageBox

import StreamHider
import PythonInterop
import Utils

continueDecrypt image curveKey (Just decodedContent) = do
    workingDirectory <- promptFolder "Where would you like to run the Python script?"
    let scriptName = "script.py"
    let fullScriptPath = FP.joinPath [workingDirectory, scriptName]

    BL.writeFile fullScriptPath decodedContent

    success <- runPythonInDirectory workingDirectory scriptName

    SD.removeFile fullScriptPath

    case success of
        True -> showSuccess "The program executed successfully!"
        False -> showError "The program failed with an error. Try again?"
continueDecrypt image curveKey Nothing = showError "This image does not contain any decryptable data!"

startDecrypt (Just image) (Just curveKey) = do
    let decoded = decodeStringFromImage image
    let decodedContent = decryptFile curveKey decoded

    continueDecrypt image curveKey decodedContent
startDecrypt Nothing curvekey = showError "This file does not contain any image data!"
startDecrypt image Nothing = showError "Cannot load Curve25519 key!"

promptDecrypt :: IO()
promptDecrypt = do
    file <- promptImageToDecrypt
    image <- readImageFromFile file

    curveKey <- getCurve25519Key
    startDecrypt image curveKey
    return ()

encryptionComplete :: Bool -> IO ()
encryptionComplete True = showSuccess "The image has been successfully encrypted and saved!"
encryptionComplete False = showError "This image does not have enough bandwidth to hide this file!"

startEncrypt (Just image) script saveFile (Just curveKey) = do
    nonce <- createXSalsa20Nonce
    let salsaContent = encryptFile curveKey nonce script
    saveHiddenImage image salsaContent saveFile
startEncrypt Nothing script saveFile curveKey = do
    showError "This file does not contain any image data!"
    return False
startEncrypt image script saveFile Nothing = do
    showError "Cannot load Curve25519 key!"
    return False

promptEncrypt :: IO()
promptEncrypt = do
    file <- promptImageToEncrypt
    image <- readImageFromFile file
    scriptFile <- promptScriptToRead "Which Python script would you like to hide?"
    saveFile <- promptImageToSave "What would you like to save the encrypted image as?"
    script <- BL.readFile scriptFile

    curveKey <- getCurve25519Key
    success <- startEncrypt image script saveFile curveKey

    encryptionComplete success
    return ()

promptDecryptOrEncrypt :: IO ()
promptDecryptOrEncrypt = do
    value <- showQuestion "What would you like to do?" "Would you like to decrypt an image?"

    case value of
        True -> promptDecrypt
        False -> promptEncrypt

pythonNotInstalled :: IO ()
pythonNotInstalled = do
    showError "Python is not installed on this computer!"
    WB.openBrowser "https://python.org/downloads/"
    return ()

checkPythonInstallation :: IO()
checkPythonInstallation = do
    pythonInstalled <- isPythonInstalled

    case pythonInstalled of
        True -> promptDecryptOrEncrypt
        False -> pythonNotInstalled

main :: IO ()
main = do
    checkPythonInstallation