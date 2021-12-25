import ReadFile
import Intcode (execICfull, parse)

-- Simple test running an Intcode program on a test input
main = do
    content <- ReadFile.readFileArgDefault "inputs/test"
    let prog = Intcode.parse content
    putStrLn $ show $ execICfull prog [1]
