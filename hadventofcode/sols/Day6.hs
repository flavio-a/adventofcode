import System.IO
import System.Environment
import System.Exit
import Data.Sort

type Group = [String]

makeGroups :: String -> [Group]
makeGroups content = map (map sort) $ _makeGroups $ lines content
    where
        _makeGroups :: [String] -> [Group]
        _makeGroups [] = []
        _makeGroups cusu = (takeWhile (/= "") cusu):(_makeGroups $ dropWhile (== "") $ dropWhile (/= "") cusu)
        -- _makeGroups cusu = (:) <*> (takeWhile (/= "")) (_makeGroups . dropWhile (== "") . dropWhile (/= ""))


groupUnion :: Group -> [Char]
groupUnion [] = []
groupUnion [x] = x
groupUnion (x:xs) = sortedMerge x $ groupUnion xs
    where
        sortedMerge :: Ord a => [a] -> [a] -> [a]
        sortedMerge [] x = x
        sortedMerge x [] = x
        sortedMerge lx@(x:xs) ly@(y:ys) | x < y     = x:(sortedMerge xs ly)
                                        | x == y    = x:(sortedMerge xs ys)
                                        | otherwise = y:(sortedMerge lx ys)


groupIntersection :: Group -> [Char]
groupIntersection [] = []
groupIntersection [x] = x
groupIntersection (x:xs) = sortedIntersect x $ groupIntersection xs
    where
        sortedIntersect :: Ord a => [a] -> [a] -> [a]
        sortedIntersect [] _ = []
        sortedIntersect _ [] = []
        sortedIntersect lx@(x:xs) ly@(y:ys) | x < y     = sortedIntersect xs ly
                                            | x == y    = x:(sortedIntersect xs ys)
                                            | otherwise = sortedIntersect lx ys

main = do
    args <- getArgs
    if length(args) < 1 then
        putStrLn "Error: missing file name" >> exitWith ExitSuccess
    else do
        handle <- openFile (head args) ReadMode
        content <- hGetContents handle
        let groups = makeGroups content :: [Group]
        putStrLn $ show $ sum $ map (length . groupUnion) $ groups
        putStrLn $ show $ sum $ map (length . groupIntersection) $ groups
