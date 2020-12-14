{-# LANGUAGE PatternGuards #-}
import ReadFile
import Data.Bits
import Data.List
import Data.List.Split (splitOneOf)
import Data.Function (on)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
-- for debugging
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- A mask contains two numbers, the First to .&. with the zeros and
-- the other to .|. with the ones
type MaskPair = (Int, Int)
type Address = Int
type Value = Int
type Memory = [(Address, Value)]

data Instr = Mask MaskPair | Assign Address Value
    -- deriving (Eq)

instance Show Instr where
    -- show (Mask (n0, n1)) = "Mask " ++ show n0 ++ "(" ++ showBin n0 ++ ") " ++ show n1 ++ "(" ++ showBin n1 ++ ")"
    show (Mask (n0, n1)) = "Mask " ++ showBin n0 ++ " " ++ showBin n1
        where
            showBin n = showIntAtBase 2 intToDigit n ""
    show (Assign n0 n1) = "Assign " ++ show n0 ++ " " ++ show n1

type State = (Memory, MaskPair)

parseMask1 :: String -> Instr
parseMask1 s = Mask $ foldl' go (0, 0) s
    where
        go :: (Int, Int) -> Char -> (Int, Int)
        go (n0, n1) 'X' = (2* n0 + 1, 2 * n1)
        go (n0, n1) '0' = (2* n0, 2 * n1)
        go (n0, n1) '1' = (2* n0 + 1, 2 * n1 + 1)

parseLine1 :: String -> Instr
parseLine1 line | Just mask <- stripPrefix "mask = " line = parseMask1 mask
parseLine1 line = Assign (read $ tokens !! 1) (read $ tokens !! 4)
    where
        tokens = splitOneOf "[] " line

applyMask1 :: MaskPair -> Int -> Int
applyMask1 (n0, n1) = (.&. n0) . (.|. n1)

updateState1 :: State -> Instr -> State
updateState1 (mem, _) (Mask mp) = (mem, mp)
updateState1 (mem, mp) (Assign addr val) = ((addr, applyMask1 mp val):mem, mp)

type Mask2Pair = (Int, [Int])
data Instr2 = Mask2 Mask2Pair | Assign2 Address Value
    deriving (Show)

parseMask2 :: String -> Instr2
parseMask2 s = Mask2 (n0, nxs)
    where
        (n0, _, nxs) = foldl' go (0, 35, []) s

        go :: (Int, Int, [Int]) -> Char -> (Int, Int, [Int])
        go (n0, idx, nxs) 'X' = (2 * n0, idx - 1, idx:nxs)
        go (n0, idx, nxs) '0' = (2 * n0, idx - 1, nxs)
        go (n0, idx, nxs) '1' = (2 * n0 + 1, idx - 1, nxs)

parseLine2 :: String -> Instr2
parseLine2 line | Just mask <- stripPrefix "mask = " line = parseMask2 mask
parseLine2 line = Assign2 (read $ tokens !! 1) (read $ tokens !! 4)
    where
        tokens = splitOneOf "[] " line

applyMask2 :: Mask2Pair -> Int -> [Int]
applyMask2 (n0, nXs) n = do
    let basen = n .|. n0
    foldM (\num idx -> [setBit num idx, clearBit num idx]) basen nXs


type State2 = (Memory, Mask2Pair)
updateState2 :: State2 -> Instr2 -> State2
updateState2 (mem, _) (Mask2 m) = (mem, m)
updateState2 (mem, mask) (Assign2 addr val) = ((map (flip (,) val) $ applyMask2 mask addr) ++ mem, mask)

sumMem :: Memory -> Int
sumMem = Map.foldl' (+) 0 . Map.fromList . reverse

main = do
    content <- ReadFile.readFileArg
    -- Part 1
    let instrs1 = map parseLine1 $ lines content
    let mem = fst $ foldl' updateState1 ([], (0, 0)) instrs1
    putStrLn $ show $ sumMem mem
    -- Part 2
    let instrs2 = map parseLine2 $ lines content
    -- putStrLn $ show $ foldl' updateState2 ([], (0, [])) instrs2
    let mem = fst $ foldl' updateState2 ([], (0, [])) instrs2
    putStrLn $ show $ sumMem mem
