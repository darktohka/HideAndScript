{-# LANGUAGE OverloadedStrings #-}

module GUI.FolderPrompt (
    promptFolder
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.Exit as SE

promptFolder :: String -> IO String
promptFolder title = do
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.selectFolderDialog (T.pack title) directory

    case value of
        Just directory -> return $ T.unpack directory
        Nothing -> SE.exitSuccess
