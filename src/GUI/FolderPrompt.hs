{-# LANGUAGE OverloadedStrings #-}

module GUI.FolderPrompt (
    promptFolder
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.Exit as SE

-- Prompts the user for any folder.
-- This function can only be used inside an IO Monad.
promptFolder :: String -> IO String
promptFolder title = do
    -- The current directory will be used as a default starting point.
    dir <- SD.getCurrentDirectory
    let directory = T.pack $ dir ++ "/"

    value <- TF.selectFolderDialog (T.pack title) directory

    -- Convert the value into a string
    case value of
        Just directory -> return $ T.unpack directory
        Nothing -> SE.exitSuccess
