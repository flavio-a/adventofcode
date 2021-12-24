module ReadFile (
    module Data.List.Split,
    readFileArg)
    where

import System.IO
import System.Environment
import System.Exit
import Data.List.Split (splitOn, splitOneOf)

readFileArg :: IO String
readFileArg = do
    args <- getArgs
    if length(args) < 1 then
        putStrLn "Error: missing file name" >> exitWith ExitSuccess
    else do
        handle <- openFile (head args) ReadMode
        content <- hGetContents handle
        return content
