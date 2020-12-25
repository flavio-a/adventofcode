import ReadFile
import Data.List
import Data.Sort

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- predNum :: [Int] -> Int
-- predNum nums = (length $ dropWhile (< last nums - 3) nums) - 1

predNum :: [Int] -> Int -> Int
predNum sorted i = subtract 1 $ length $ dropWhile (< last nums - 3) nums
    where
        nums = take (i + 1) (0:sorted)

countWays :: Int -> [Int] -> Int
countWays len dp = sum $ drop (length dp - len) dp

main = do
    content <- ReadFile.readFileArg
    let numbers = map read $ lines content :: [Int]
    let sorted = sort numbers
    -- Part 1
    let diffs = zipWith (-) sorted (0:sorted)
    let ones = count (== 1) diffs
    let threes = 1 + count (== 3) diffs
    putStrLn $ show $ ones * threes
    -- Part 2
    -- Not the nicest dp I've ever written, but the numbers here are
    -- so small it works anyway
    let dp = 1:[countWays (predNum sorted i) $ take i dp | i <- [1 .. length sorted]]
    -- putStrLn $ show $ dp
    putStrLn $ show $ last dp
