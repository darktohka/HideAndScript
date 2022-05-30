{-# LANGUAGE OverloadedStrings #-}
module PythonInterop (
    isPythonInstalled,
    runPythonInDirectory
) where

import qualified System.Process.Typed as PR

-- Checks whether the system has a working Python installation
-- This is done by querying Python's version
-- If this fails, Python is not installed properly
isPythonInstalled :: IO Bool
isPythonInstalled = do
    exitCode <- PR.runProcess "python --version"
    return (exitCode == PR.ExitSuccess)

-- Runs a Python script inside a given directory
-- Relies on the system installation of Python
runPythonInDirectory :: FilePath -> FilePath -> IO Bool
runPythonInDirectory directory scriptName = do
    exitCode <- PR.runProcess processWithDirectory
    return (exitCode == PR.ExitSuccess)
    where
        -- All Python scripts run in optimized (-OO) mode
        process = PR.proc "python" ["-OO", scriptName]
        -- A new console must be created since our application does not have one
        processWithConsole = PR.setCreateNewConsole True process
        -- The process must run in a different working directory
        processWithDirectory = PR.setWorkingDir directory processWithConsole
