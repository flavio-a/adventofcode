import ReadFile
import Intcode (execIC, parse)

main = do
    content <- ReadFile.readFileArg
    let prog = Intcode.parse content
    -- Part 1
    putStrLn $ show $ last $ execIC prog [1]
    -- Part 2
    putStrLn $ show $ last $ execIC prog [5]