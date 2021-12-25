module ReadFile (
    module Data.List.Split,
    readFileArg,
    readFileArgDefault,
) where

import System.IO
import System.Environment
import System.Exit
import Data.List.Split (splitOn, splitOneOf)

readFileArg :: IO String
readFileArg = do
    args <- getArgs
    if length args < 1 then
        putStrLn "Error: missing file name" >> exitWith ExitSuccess
    else do
        handle <- openFile (head args) ReadMode
        content <- hGetContents handle
        return content

readFileArgDefault :: String -> IO String
readFileArgDefault defaultfilename = do
    args <- getArgs
    let filename = if length args < 1 then defaultfilename else head args
    handle <- openFile filename ReadMode
    hGetContents handle
