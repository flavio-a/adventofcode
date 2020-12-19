import ReadFile
import Data.Maybe
import qualified Data.IntMap.Strict as Map

type Store = Map.IntMap Int
nextVal :: Store -> Int -> Int -> Int
nextVal store pos v = case Map.lookup v store of
        Just idx -> pos - idx
        Nothing  -> 0

getNum :: [Int] -> Int -> Int
getNum startingnumbers target = go firststore firstidx (last startingnumbers)
    where
        firststore :: Store
        firststore = Map.fromList $ zip startingnumbers [1..]

        firstidx = length startingnumbers

        go :: Store -> Int -> Int -> Int
        go store pos val | pos >= target = newval
                         | otherwise     = go store' (pos + 1) newval
            where
                newval = nextVal store pos val
                store' = Map.insert val pos store

main = do
    content <- ReadFile.readFileArg
    let startingnumbers = map read $ splitOn "," content :: [Int]
    -- Part 1
    putStrLn $ show $ getNum startingnumbers (2020 - 1) -- Zero basedness
    -- Part 2
    putStrLn $ show $ getNum startingnumbers (30000000 - 1) -- Zero basedness
