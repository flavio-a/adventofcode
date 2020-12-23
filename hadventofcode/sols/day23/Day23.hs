import ReadFile
import Data.List
import Data.Maybe
import Data.Function (on)
import Data.List.Split (splitWhen)
import Data.Char (digitToInt)

import Debug.Trace

type Board = [Int]

parseInput :: String -> Board
parseInput str = map digitToInt str

doMove :: Board -> Board
doMove lst = bef ++ [next] ++ three ++ aft ++ [curr]
    where
        curr = head lst
        (three, rest) = splitAt 3 $ tail lst
        next = findNext curr
        [bef, aft] = splitWhen (== next) rest

        findNext :: Int -> Int
        findNext n = if not (n' `elem` three) then n' else findNext n'
            where
                n' = (n - 2 + length lst) `rem` length lst + 1

main = do
    content <- ReadFile.readFileArg
    let startingBoard = parseInput $ head $ lines content
    -- Part1
    let [bef, aft] = splitWhen (== 1) $ iterate doMove startingBoard !! 100
    putStrLn $ concatMap show $ aft ++ bef
    -- Part 2
    let startingBoard2 = startingBoard ++ [length startingBoard + 1..1000000]
    let [_, afterOne] = splitWhen (== 1) $ iterate doMove startingBoard2 !! 100
    putStrLn $ concatMap show $ take 2 afterOne
