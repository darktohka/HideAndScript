{-# LANGUAGE OverloadedStrings #-}

module GUI.PythonPrompt (
    promptScriptToRead
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.Exit as SE

promptScriptToRead :: String -> IO String
promptScriptToRead title = do
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.openFileDialog (T.pack title) directory ["*.py"] "Python Scripts (*.py)" False

    case value of
        Just [file] -> return $ T.unpack file
        Nothing -> SE.exitSuccess
