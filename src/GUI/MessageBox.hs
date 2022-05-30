{-# LANGUAGE OverloadedStrings #-}

module GUI.MessageBox (
    showError,
    showSuccess,
    showQuestion
) where

import qualified Graphics.UI.TinyFileDialogs as TF
import qualified Data.Text as T

-- Shows a message box with an error icon and a custom message.
-- This function can only be used inside an IO Monad.
showError :: String -> IO ()
showError message = do
    TF.messageBox "Error!" (T.pack message) TF.Error TF.OK
    return ()

-- Shows a message box with an information icon and a custom message.
-- This function can only be used inside an IO Monad.
showSuccess :: String -> IO ()
showSuccess message = do
    TF.messageBox "Congratulations!" (T.pack message) TF.Info TF.OK
    return ()

-- Shows a question dialog with a custom title and message.
-- Returns a True value if the Yes button is pressed.
-- Returns a False value if the No button is pressed.
-- This function can only be used inside an IO Monad.
showQuestion :: String -> String -> IO Bool
showQuestion title message = do
    value <- TF.messageBox (T.pack title) (T.pack message) TF.Question TF.YN_Yes

    -- Convert the TinyFileDialogs-specific return result into a Bool
    case value of
        TF.YN_Yes -> return True
        _ -> return False
