module Main where

import System.IO
import System.Process
import System.Exit (ExitCode(..))

import System.Directory (setCurrentDirectory)

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
  | command == "cd" = changeDirectory args
  | otherwise       = executeExternalCommand (command:args)

changeDirectory :: [String] -> IO ExitCode
changeDirectory [] = do
  putStrLn "Usage: cd <directory>"
  pure $ ExitFailure 1
changeDirectory (dir:_) = do
  setCurrentDirectory dir
  pure ExitSuccess

executeExternalCommand :: [String] -> IO ExitCode
executeExternalCommand commandArgs = do
  -- Check for I/O redirect
  let (commandArgs', inputRedir, outputRedir) = processRedirection commandArgs

  let process = (proc (head commandArgs') (tail commandArgs'))
                  { std_in = inputRedir
                  , std_out = outputRedir
                  , std_err = Inherit -- Inherit standard error
                  }
  (_, _, _, processHandle) <- createProcess process
  waitForProcess processHandle

processRedirection :: [String] -> ([String], Maybe Handle, Maybe Handle)
processRedirection args = processRedirection' args Nothing Nothing
  where
    processRedirection' [] inRedir outRedir = ([], inRedir, outRedir)
    processRedirection' (arg:rest) inRedir outRedir
      | arg == "<" = case rest of
                       (inputFile:xs) -> processRedirection' xs (Just =<< openFile inputFile ReadMode) outRedir
                       _              -> error "Missing input file for redirection"
      | arg == ">" = case rest of
                       (outputFile:xs) -> processRedirection' xs inRedir (Just =<< openFile outputFile WriteMode)
                       _               -> error "Missing output file for redirection"

      | otherwise  = case inRedir of
                       Just h  -> case outRedir of
                                    Just h' -> ([arg], Just h, Just h')
                                    Nothing -> ([arg], Just h, Nothing)
                       Nothing -> case outRedir of
                                    Just h' -> ([arg], Nothing, Just h')
                                    Nothing -> let (args', inRedir', outRedir') = processRedirection' rest inRedir outRedir
                                               in (arg:args', inRedir', outRedir')
