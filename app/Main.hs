module Main where

import System.IO
import System.Process (createProcess, waitForProcess, proc, runProcess, readProcess, shell, callCommand, StdStream(..), CreateProcess(..), CmdSpec(..))
import System.Exit (ExitCode(..))

import System.Directory (setCurrentDirectory, getCurrentDirectory, doesFileExist, getHomeDirectory)

import System.FilePath ((</>))

import System.Environment (getEnv, setEnv, lookupEnv)

import Control.Monad (when, void, liftM)

import System.Exit (exitSuccess)

import Data.Maybe (fromMaybe)

import Control.Exception (catch, SomeException)

import Data.List (isInfixOf, isPrefixOf, stripPrefix)

data RedirectionType = InputRedirect | OutputRedirect

main :: IO ()
main = do
  putStrLn "Welcome to your shell!"
  hakshrcExists <- doesFileExist ".hakshrc"
  when hakshrcExists loadHakshrc
  loop

loadHakshrc :: IO ()
loadHakshrc = do
  currentDirHakshrcExists <- doesFileExist ".hakshrc"
  homeDir <- getHomeDirectory
  homeDirHakshrcExists <- doesFileExist (homeDir </> ".hakshrc")

  case (currentDirHakshrcExists, homeDirHakshrcExists) of
    (True, _) -> processConfigFile ".hakshrc"
    (_, True) -> processConfigFile (homeDir </> ".hakshrc")
    _         -> putStrLn "No .hakshrc found."  -- No .hakshrc found

processConfigFile :: FilePath -> IO ()
processConfigFile configFile = do
  hakshrcContents <- readFile configFile
  mapM_ processConfigLine (lines hakshrcContents)

processConfigLine :: String -> IO ()
processConfigLine line
  | isComment line = pure ()  -- Skip comment lines
  | null (stripLeadingWhitespace line) = pure ()  -- Handle empty lines
  | isAlias line   = processAlias line
  | isEnvVar line  = processEnvVar line
  | otherwise      = void $ executeExternalCommand (words line)

isComment :: String -> Bool
isComment line = "#" `isPrefixOf` stripLeadingWhitespace line

isAlias :: String -> Bool
isAlias line = "alias " `isPrefixOf` line

isEnvVar :: String -> Bool
isEnvVar line = "=" `isInfixOf` line

stripLeadingWhitespace :: String -> String
stripLeadingWhitespace = dropWhile (== ' ')

processAlias :: String -> IO ()
processAlias line = do
  let (alias, cmd) = splitAlias line
  when (not (null alias) && not (null cmd)) $ do
    putStrLn $ "Adding alias: " ++ alias ++ " -> " ++ cmd
    setAlias alias cmd

splitAlias :: String -> (String, String)
splitAlias line = case stripPrefix "alias " line of
  Just rest -> span (/= ' ') rest
  Nothing   -> ("", "")

setAlias :: String -> String -> IO ()
setAlias alias cmd = setEnv alias cmd


getAlias :: String -> IO (Maybe String)
getAlias alias = do
  maybeValue <- lookupEnv alias
  return maybeValue

processEnvVar :: String -> IO ()
processEnvVar line = do
  let (var, value) = splitEnvVar line
  when (not (null var) && not (null value)) $ do
    putStrLn $ "Setting environment variable: " ++ var ++ "=" ++ value
    setEnvVar var value

splitEnvVar :: String -> (String, String)
splitEnvVar line = case break (== '=') line of
  (var, '=':value) -> (var, value)
  _                -> ("", "")

setEnvVar :: String -> String -> IO ()
setEnvVar var value = setEnv var value


loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  input <- getLine
  -- TODO user input
  if input == "test"
    then putStrLn "wow"
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
  | null command = pure ExitSuccess  -- Handle empty input
  | command == "cd"   = changeDirectory args
  | command == "pwd"  = printWorkingDirectory
  | command == "echo" = echoCommand args
  | otherwise         = do
      aliasCommand <- getAlias command
      case aliasCommand of
        Just cmd -> do
          putStrLn $ "Executing alias: " ++ cmd ++ " " ++ unwords args
          (_, _, _, processHandle) <- createProcess (proc (head $ words cmd) (words $ unwords args)) {
            cwd = Nothing,   -- Set the working directory to Nothing to use the current directory
            std_out = Inherit,  -- Set stdout to inherit to print command output directly
            std_err = Inherit,  -- Set stderr to inherit to print command errors directly
            env = Nothing    -- Set the environment to Nothing to use the current environment
          }
          waitForProcess processHandle
        Nothing  -> do
          putStrLn $ "Executing command: " ++ command ++ " " ++ unwords args
          (_, _, _, processHandle) <- createProcess (proc command args) {
            cwd = Nothing,   -- Set the working directory to Nothing to use the current directory
            std_out = Inherit,  -- Set stdout to inherit to print command output directly
            std_err = Inherit,  -- Set stderr to inherit to print command errors directly
            env = Nothing    -- Set the environment to Nothing to use the current environment
          }
          waitForProcess processHandle
        `catch` handleCommandError

handleCommandError :: SomeException -> IO ExitCode
handleCommandError ex = do
  putStrLn $ "Error executing command: " ++ show ex
  pure $ ExitFailure 1

printWorkingDirectory :: IO ExitCode
printWorkingDirectory = do
  cwd <- getCurrentDirectory
  putStrLn cwd
  pure ExitSuccess

expandEnvironmentVariable :: String -> IO String
expandEnvironmentVariable ('$' : var) = do
  value <- lookupEnv (tail var)
  return $ fromMaybe ("$" ++ var) value
expandEnvironmentVariable arg = return arg

echoCommand :: [String] -> IO ExitCode
echoCommand args = do
  expandedArgs <- mapM expandEnvironmentVariable args
  putStrLn (unwords expandedArgs)
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
