{-# LANGUAGE OverloadedStrings #-}

module GUI.PythonPrompt (
    promptScriptToRead
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.Exit as SE

-- Prompts the user for a Python script to read.
-- This function can only be used inside an IO Monad.
promptScriptToRead :: String -> IO String
promptScriptToRead title = do
    -- The current directory will be used as a default starting point.
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.openFileDialog (T.pack title) directory ["*.py"] "Python Scripts (*.py)" False

    -- Convert the file path to a string
    case value of
        Just [file] -> return $ T.unpack file
        Nothing -> SE.exitSuccess
