import ReadFile
import Intcode (ICProg, execIC, parse, execICfull)
import Data.List (foldl', permutations)

runPhaseSeq :: ICProg -> [Int] -> Int
runPhaseSeq prog = foldl' (\outval phase -> head $ execIC prog [phase, outval]) 0

output :: ICProg -> [Int] -> Int
output prog phaseSeq = last $ last outputs
    where
        outputs = map func $ zip [0..] phaseSeq
        func (idx, phase) = execIC prog $ phase:(case idx of
                0 -> 0:(last outputs)
                _ -> (outputs !! (idx - 1))
            )

main = do
    content <- ReadFile.readFileArg
    let prog = Intcode.parse content
    -- Part 1
    putStrLn $ show $ maximum $ map (runPhaseSeq prog) $ permutations [0..4]
    -- Part 2
    putStrLn $ show $ maximum $ map (output prog) $ permutations [5..9]
