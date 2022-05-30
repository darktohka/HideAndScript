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

-- DECRYPTION

-- Prompts the user for all information necessary for decryption.
promptDecrypt :: IO ()
promptDecrypt = do
    file <- promptImageToDecrypt

    -- Read the image and key.
    image <- readImageFromFile file
    curveKey <- getCurve25519Key

    -- Start the decryption process!
    startDecrypt image curveKey
    return ()

-- Starts the decryption process.
-- We have to check if the image and the key has been loaded properly.
startDecrypt (Just image) (Just curveKey) = do
    -- Decode the bytestring from the image...
    let decoded = decodeStringFromImage image

    -- Attempt to decryrpt the content...
    let decodedContent = decryptFile curveKey decoded

    -- Decide what to do next.
    continueDecrypt image curveKey decodedContent
startDecrypt Nothing curvekey = showError "This file does not contain any image data!"
startDecrypt image Nothing = showError "Cannot load Curve25519 key!"

-- Continues the decryption process.
-- We have to decide what to do now, depending on the success value.
continueDecrypt image curveKey (Just decodedContent) = do
    -- Ask the user where to start the script.
    workingDirectory <- promptFolder "Where would you like to run the Python script?"

    -- Unpack the decrypted script into a file.
    let scriptName = "script.py"
    let fullScriptPath = FP.joinPath [workingDirectory, scriptName]

    BL.writeFile fullScriptPath decodedContent

    -- Run the decrypted script, and keep track of the success value.
    success <- runPythonInDirectory workingDirectory scriptName

    -- Remove the decrypted script, as it is no longer necessary...
    SD.removeFile fullScriptPath

    -- Show a friendly message to the user!
    case success of
        True -> showSuccess "The program executed successfully!"
        False -> showError "The program failed with an error. Try again?"
continueDecrypt image curveKey Nothing = showError "This image does not contain any decryptable data!"

-- ENCRYPTION

-- Prompts the user for all information necessary for encryption.
promptEncrypt :: IO ()
promptEncrypt = do
    file <- promptImageToEncrypt
    image <- readImageFromFile file
    scriptFile <- promptScriptToRead "Which Python script would you like to hide?"
    saveFile <- promptImageToSave "What would you like to save the encrypted image as?"

    -- Read the script file from the disk.
    script <- BL.readFile scriptFile

    -- Attempt to encrypt the image...
    curveKey <- getCurve25519Key
    success <- startEncrypt image script saveFile curveKey

    -- Let the user know
    encryptionComplete success
    return ()

-- Starts the image encryption process.
-- We have to check if the image and the key has been loaded properly.
startEncrypt (Just image) script saveFile (Just curveKey) = do
    -- Create a new nonce, and encrypt the content!
    nonce <- createXSalsa20Nonce
    let salsaContent = encryptFile curveKey nonce script

    -- Save the encrypted content into the file!
    saveHiddenImage image salsaContent saveFile
startEncrypt Nothing script saveFile curveKey = do
    showError "This file does not contain any image data!"
    return False
startEncrypt image script saveFile Nothing = do
    showError "Cannot load Curve25519 key!"
    return False

-- Show a friendly message to the user if the encryption completed successfully or not.
encryptionComplete :: Bool -> IO ()
encryptionComplete True = showSuccess "The image has been successfully encrypted and saved!"
encryptionComplete False = showError "This image does not have enough bandwidth to hide this file!"

-- PROMPTS

-- Asks the user whether they'd like to decrypt or encrypt an image.
promptDecryptOrEncrypt :: IO ()
promptDecryptOrEncrypt = do
    value <- showQuestion "What would you like to do?" "Welcome to HideAndScript!\n\nWould you like to decrypt an image?\nIf so, press the Yes button.\n\nIf you would like to encrypt an image, press the No button."

    case value of
        True -> promptDecrypt
        False -> promptEncrypt

-- Tells the user that their Python installation is missing.
pythonNotInstalled :: IO ()
pythonNotInstalled = do
    -- Show the user a friendly message, letting them know that
    -- Python is not installed.
    showError "Python is not installed on this computer!"

    -- Open their browser to the Python download page.
    WB.openBrowser "https://python.org/downloads/"

    return ()

-- Checks the system for a Python installation.
-- If an installation is found, the program continues.
checkPythonInstallation :: IO ()
checkPythonInstallation = do
    -- Is Python installed?
    pythonInstalled <- isPythonInstalled

    case pythonInstalled of
        -- If installed, prompt the user if they want to decrypt.
        True -> promptDecryptOrEncrypt
        -- If not installed, send the user to the download screen.
        False -> pythonNotInstalled

-- The entry point of the program.
-- First, check if Python is installed.
main :: IO ()
main = do
    checkPythonInstallation
