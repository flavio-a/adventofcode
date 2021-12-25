import ReadFile
import Intcode (execIC, parse)
import Data.List (foldl', permutations)

main = do
    content <- ReadFile.readFileArgDefault "inputs/day9"
    let prog = Intcode.parse content
    -- Part 1
    putStrLn $ show $ last $ execIC prog [1]
    -- Part 2
    putStrLn $ show $ last $ execIC prog [2]
