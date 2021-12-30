import ReadFile
import Intcode
import Data.Char (chr, ord)
import Control.Monad
import Control.Monad.Loops

repeatIC :: ICState -> IO ICState
repeatIC vm = do
    command <- getLine
    let (output, newvm) = execICinput vm $ map ord (command ++ "\n")
    putStr $ map chr output
    return newvm

main = do
    content <- ReadFile.readFileArgDefault "inputs/day25"
    let prog = Intcode.parse content
    -- Part 1
    -- This is actually just an interface to play the game coded in the input
    let (output, vm) = execICinput (initICvm prog []) []
    putStr $ map chr output
    iterateM_ repeatIC vm