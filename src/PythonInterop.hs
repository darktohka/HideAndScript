{-# LANGUAGE OverloadedStrings #-}
module PythonInterop (
    isPythonInstalled,
    runPythonInDirectory
) where

import qualified System.Process.Typed as PR

isPythonInstalled :: IO Bool
isPythonInstalled = do
    exitCode <- PR.runProcess "python --version"
    return (exitCode == PR.ExitSuccess)

runPythonInDirectory :: FilePath -> FilePath -> IO Bool
runPythonInDirectory directory scriptName = do
    exitCode <- PR.runProcess (PR.setWorkingDir directory (PR.setCreateNewConsole True (PR.proc "python" ["-OO", scriptName])))
    return (exitCode == PR.ExitSuccess)
