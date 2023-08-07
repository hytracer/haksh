module Main where

import System.IO
import System.Process
import System.Exit (ExitCode(..))

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
executeCommand (command:args) = do
  -- Create process
  (_, _, _, processHandle) <- createProcess (proc command args)
  -- Wait for process term
  waitForProcess processHandle
