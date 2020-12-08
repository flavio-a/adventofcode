module AoCAssembly (
    module Data.Array,
    Instruction (..),
    Program,
    Accumulator,
    ProgState,
    -- parseLine,
    parseProgram,
    execInstr,
    execInstrState,
    ) where

import Data.Array

data Instruction = Nop Int | Acc Int | Jmp Int
    deriving (Eq, Ord, Show)
type Program = Array Int Instruction
type Accumulator = Int
type ProgState = (Accumulator, Int)

parseLine :: String -> Instruction
parseLine line = case take 3 line of "nop" -> Nop v
                                     "acc" -> Acc v
                                     "jmp" -> Jmp v
    where
        vstring = drop 4 line :: String
        v = if head vstring == '+'
            then read $ tail vstring
            else read vstring

parseProgram :: String -> Program
parseProgram text = listArray (0, length instructions - 1) instructions
    where
        instructions = map parseLine $ lines text

-- Execute a single instruction given current accumulator
-- Returns the new accumulator and the relative position of the next
-- instruction to execute
execInstr :: Instruction -> Accumulator -> (Accumulator, Int)
execInstr (Nop _) acc = (acc, 1)
execInstr (Acc v) acc = (acc + v, 1)
execInstr (Jmp v) acc = (acc, v)


execInstrState :: Instruction -> ProgState -> ProgState
execInstrState instr (acc, idx) = (acc', idx + delta)
    where
        (acc', delta) = execInstr instr acc
