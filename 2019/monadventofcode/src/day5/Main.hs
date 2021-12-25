import ReadFile
import Intcode (execIC, parse)

main = do
    content <- ReadFile.readFileArgDefault "inputs/day5"
    let prog = Intcode.parse content
    -- Part 1
    putStrLn $ show $ last $ execIC prog [1]
    -- Part 2
    putStrLn $ show $ last $ execIC prog [5]