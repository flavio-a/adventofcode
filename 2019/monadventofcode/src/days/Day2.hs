import ReadFile
import Intcode

main = do
    content <- ReadFile.readFileArg
    let a = Intcode.parse content
    -- Part 1
    putStrLn $ show $ execIC (a // [(1, 12), (2, 2)])
    -- Part 2
    let (noun, verb) = head [(noun, verb) |
            noun <- [0..100],
            verb <- [0..100],
            19690720 == execIC (a // [(1, noun), (2, verb)])
            ]
    putStrLn $ show $ 100 * noun + verb