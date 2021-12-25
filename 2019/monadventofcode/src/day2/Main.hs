import ReadFile
import Intcode (execICfull, parse, (//), (!))

-- execICfirst :: ICProg -> Int
execICfirst prog = (! 0) $ fst $ execICfull prog [0..]

main = do
    content <- ReadFile.readFileArg
    let a = Intcode.parse content
    -- Part 1
    putStrLn $ show $ execICfirst (a // [(1, 12), (2, 2)])
    -- Part 2
    let (noun, verb) = head [(noun, verb) |
            noun <- [0..100],
            verb <- [0..100],
            19690720 == execICfirst (a // [(1, noun), (2, verb)])
            ]
    putStrLn $ show $ 100 * noun + verb