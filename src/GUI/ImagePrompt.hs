{-# LANGUAGE OverloadedStrings #-}

module GUI.ImagePrompt (
    promptImageToRead,
    promptImageToSave,
    promptImageToDecrypt,
    promptImageToEncrypt
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.Exit as SE

-- Prompts the user for an image to read.
-- This function can only be used inside an IO Monad.
promptImageToRead :: String -> IO String
promptImageToRead title = do
    -- The current directory will be used as a default starting point.
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.openFileDialog (T.pack title) directory ["*.jpg", "*.jpeg", "*.png"] "Image Files (*.jpg, *.jpeg, *.png)" False

    -- Convert the value into a string
    case value of
        Just [file] -> return $ T.unpack file
        Nothing -> SE.exitSuccess

-- Prompts the user for an image to save.
-- This function can only be used inside an IO Monad.
promptImageToSave :: String -> IO String
promptImageToSave title = do
    -- A file called encrypted.png will be used as a default starting point.
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/encrypted.png"

    value <- TF.saveFileDialog (T.pack title) directory ["*.png"] "Image Files (*.png)"

    -- Convert the value into a string
    case value of
        Just file -> return $ T.unpack file
        Nothing -> SE.exitSuccess

-- Prompts the user for an image to decrypt.
-- This function can only be used inside an IO Monad.
promptImageToDecrypt :: IO String
promptImageToDecrypt = promptImageToRead "Choose an image to decrypt!"

-- Prompts the user for an image to encrypt.
-- This function can only be used inside an IO Monad.
promptImageToEncrypt :: IO String
promptImageToEncrypt = promptImageToRead "Choose an image to encrypt!"
