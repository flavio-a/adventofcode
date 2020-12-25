import ReadFile
import AoCAssembly
import AoCAssemblyStateful
import Data.Tuple.Extra

-- Utilities
changeProgAt :: Program -> Int -> Program
changeProgAt prog i = prog'
    where
        istr = prog ! i
        prog' = prog // [(i, case istr of Nop v -> Jmp v
                                          Jmp v -> Nop v)]

isAcc :: Instruction -> Bool
isAcc (Acc _) = True
isAcc _ = False

isInRange :: Program -> MachineState -> Bool
isInRange prog (_, idx) = inRange (bounds prog) idx


-- Explicit state version
runProgChecked :: Program -> [Int] -> MachineState -> MachineState
runProgChecked prog visited state@(acc, idx) =
    if (idx `elem` visited) || (not $ inRange (bounds prog) idx) then state else
        let state' = execInstrState (prog ! idx) state
        in runProgChecked prog (idx:visited) state'


-- State monad version
-- Results True when the program terminates correctly, False if it enters an
-- endless loop
runProgCheckedASM :: Program -> State (Accumulator, Int, [Int]) Bool
runProgCheckedASM prog = do
    (acc, idx, visited) <- get
    let (acc', delta) = execInstr (prog ! idx) acc
    let idx' = idx + delta
    put (acc', idx', idx:visited)
    if (idx' `elem` visited) then
        return False
    else if not $ inRange (bounds prog) idx' then
            return True
        else
            runProgCheckedASM prog


-- Main
main = do
    content <- ReadFile.readFileArg
    let prog = parseProgram content
    -- Explicit state
    putStrLn $ show $ fst $ runProgChecked prog [] (0, 0)
    putStrLn $ show $ fst $ head
        $ filter (not . isInRange prog)
        $ [runProgChecked (changeProgAt prog i) [] (0, 0) |
            i <- [0..rangeSize (bounds prog)],
            not $ isAcc (prog ! i) ]

    -- State monad
    putStrLn $ show
        $ fst3 $ execState (runProgCheckedASM prog) (0, 0, [])

    putStrLn $ show
        $ head
        $ map (fst3 . snd)
        $ filter fst
        $ map (flip runState $ (0, 0, []))
        $ [runProgCheckedASM (changeProgAt prog i) |
            i <- range (bounds prog),
            not $ isAcc (prog ! i)]
