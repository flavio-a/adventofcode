import ReadFile
import Intcode
import Data.Char (chr, ord)
import Control.Monad

main = do
    content <- ReadFile.readFileArgDefault "inputs/day21"
    let prog = Intcode.parse content
    -- Part 1

    -- My program is (pseudocode)
    -- IF (one of A,B,C is empty and D is full) { JUMP } else { don't JUMP }
    -- that translates in
    -- JUMP = (-A or -B or -C) and D
    let p1program = unlines $ [ "NOT A J"
                              , "NOT B T"
                              , "OR  T J"
                              , "NOT C T"
                              , "OR  T J"
                              , "AND D J"
                              , "WALK" ]
    let output = execIC prog $ map ord p1program
    let output' = dropWhile (< 256) output
    if null output' then
        putStrLn $ map chr output
    else
        putStrLn $ show $ head output'
    -- Part 2

    -- My program is -(A ∧ B ∧ C) ∧ D ∧ (H v (E ∧ (I v F))) that is it jumps
    -- only if needed (an empty in the three in front), there's room to land,
    -- and from D there's a way to leave in the immediate (ie. can jump from D
    -- and land in H or can jump from the next tile E and land in I)
    let p2program = unlines $ [ "NOT A J"
                              , "NOT J J"
                              , "AND B J"
                              , "AND C J"
                              , "NOT J J"
                              -- J == -(A ∧ B ∧ C)
                              , "AND D J"
                              , "NOT F T"
                              , "NOT T T"
                              , "OR  I T"
                              -- T == I v F
                              , "AND E T"
                              , "OR  H T"
                              , "AND T J"
                              , "RUN" ]
    let output = execIC prog $ map ord p2program
    let output' = dropWhile (< 256) output
    if null output' then
        putStrLn $ map chr output
    else
        putStrLn $ show $ head output'
