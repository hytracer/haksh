module Main where

import System.IO
import System.Process
import System.Exit (ExitCode(..))

import System.Directory (setCurrentDirectory, getCurrentDirectory)

data RedirectionType = InputRedirect | OutputRedirect

main :: IO ()
main = do
  putStrLn "Welcome to haksh!"
  loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  input <- getLine
  -- TODO user input
  if input == "exit"
    then putStrLn "Goodbye"
    else do
      let commandTokens = words input
      exitCode <- executeCommand commandTokens
      case exitCode of
        ExitSuccess   -> loop
        ExitFailure _ -> putStrLn "Command failed"
  loop

executeCommand :: [String] -> IO ExitCode
executeCommand [] = pure ExitSuccess
executeCommand (command:args)
  | command == "cd"   = changeDirectory args
  | command == "pwd"  = printWorkingDirectory
  | command == "echo" = echoCommand args
  | otherwise         = executeExternalCommand (command:args)

printWorkingDirectory :: IO ExitCode
printWorkingDirectory = do
  cwd <- getCurrentDirectory
  putStrLn cwd
  pure ExitSuccess

echoCommand :: [String] -> IO ExitCode
echoCommand args = do
  putStrLn (unwords args)
  pure ExitSuccess

changeDirectory :: [String] -> IO ExitCode
changeDirectory [] = do
  putStrLn "Usage: cd <directory>"
  pure $ ExitFailure 1
changeDirectory (dir:_) = do
  setCurrentDirectory dir
  pure ExitSuccess

executeExternalCommand :: [String] -> IO ExitCode
executeExternalCommand commandArgs = do
  (_, _, _, processHandle) <- createProcess (proc (head commandArgs) (tail commandArgs))
  waitForProcess processHandle
