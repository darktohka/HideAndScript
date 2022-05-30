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

promptImageToRead :: String -> IO String
promptImageToRead title = do
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.openFileDialog (T.pack title) directory ["*.jpg", "*.jpeg", "*.png"] "Image Files (*.jpg, *.jpeg, *.png)" False

    case value of
        Just [file] -> return $ T.unpack file
        Nothing -> SE.exitSuccess

promptImageToSave :: String -> IO String
promptImageToSave title = do
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/encrypted.png"

    value <- TF.saveFileDialog (T.pack title) directory ["*.png"] "Image Files (*.png)"

    case value of
        Just file -> return $ T.unpack file
        Nothing -> SE.exitSuccess

promptImageToDecrypt :: IO String
promptImageToDecrypt = promptImageToRead "Choose an image to decrypt!"

promptImageToEncrypt :: IO String
promptImageToEncrypt = promptImageToRead "Choose an image to encrypt!"
