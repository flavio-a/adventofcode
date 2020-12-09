import ReadFile
import Data.List
import Data.Maybe
-- import Data.Array

apply2way :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
apply2way b f g x = f x `b` g x

-- Build segments of fixed length of the given list
-- TODO: improve this
--   - make it tail recursive
--   - avoid recomputing length
segments :: Int -> [a] -> [[a]]
segments len xs | length xs >= len  = (take len xs):(segments len $ tail xs)
                | otherwise         = []

isSum :: (Eq a, Num a) => a -> [a] -> Bool
isSum n list = n `elem` [ x + y | (x, i) <- enumlist, (y, j) <- enumlist, i /= j]
    where
        enumlist = zip list [1..]

pack :: (Eq a, Num a) => Int -> [a] -> [(a, [a])]
-- pack n list = zip (drop n list) (segments n list)
pack n = apply2way zip (drop n) (segments n)

main = do
    content <- ReadFile.readFileArg
    let numbers = map read $ lines content :: [Int]
    let preamblelen = 25
    let bad = head $ [n | (n, prefix) <- pack preamblelen numbers,
                          not $ n `isSum` prefix]
    putStrLn $ show $ bad

    -- Bad, O(n^3) solution
    let substrs = filter ((> 1) . length) $ concatMap (tail . inits) $ tails numbers
    let sums = zip (map sum substrs) substrs
    let subseq = snd $ fromJust $ find ((== bad) . fst) sums
    putStrLn $ show $ apply2way (+) minimum maximum $ subseq
