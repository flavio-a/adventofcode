import ReadFile
import AoCAssembly

runProgChecked :: Program -> [Int] -> ProgState -> ProgState
runProgChecked prog visited state@(acc, idx) =
    if (idx `elem` visited) || (not $ inRange (bounds prog) idx) then state else
        let state' = execInstrState (prog ! idx) state
        in runProgChecked prog (idx:visited) state'

changeProgAt :: Program -> Int -> Program
changeProgAt prog i = prog'
    where
        istr = prog ! i
        prog' = prog // [(i, case istr of Nop v -> Jmp v
                                          Jmp v -> Nop v)]

isAcc :: Instruction -> Bool
isAcc (Acc _) = True
isAcc _ = False

isInRange :: Program -> ProgState -> Bool
isInRange prog (_, idx) = idx >= rangeSize (bounds prog)

main = do
    content <- ReadFile.readFileArg
    let prog = parseProgram content
    putStrLn $ show $ fst $ runProgChecked prog [] (0, 0)
    putStrLn $ show $ fst $ head $ filter (isInRange prog)
        $ [runProgChecked (changeProgAt prog i) [] (0, 0)
           | i <- [0..rangeSize (bounds prog)],
             not $ isAcc (prog ! i) ]
