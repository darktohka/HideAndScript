{-# LANGUAGE OverloadedStrings #-}

module GUI.MessageBox (
    showError,
    showSuccess,
    showQuestion
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T

showError :: String -> IO ()
showError message = do
    TF.messageBox "Error!" (T.pack message) TF.Error TF.OK
    return ()

showSuccess :: String -> IO ()
showSuccess message = do
    TF.messageBox "Congratulations!" (T.pack message) TF.Info TF.OK
    return ()

showQuestion :: String -> String -> IO Bool
showQuestion title message = do
    value <- TF.messageBox (T.pack title) (T.pack message) TF.Question TF.YN_Yes

    case value of
        TF.YN_Yes -> return True
        _ -> return False
