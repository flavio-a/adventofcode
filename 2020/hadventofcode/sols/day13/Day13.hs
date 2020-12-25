import ReadFile
import Data.List.Split
import Data.Tuple.Extra

parseTimes1 :: String -> [Int]
parseTimes1 = map read . filter (/= "x") . splitOn ","

parseTimes2 :: String -> [(Integer, Integer)]
parseTimes2 = map (second read) . filter ((/= "x") . snd) . zip [0..] . splitOn ","

-- From https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a

main = do
    content <- ReadFile.readFileArg
    let contentlines = lines content
    -- Part 1
    let starttime = read $ head contentlines :: Int
    let ids = parseTimes1 $ (contentlines !! 1)
    let distances = map ((-) <*> (starttime `mod`)) ids
    putStrLn $ show $ uncurry (*) $ minimum $ zip distances ids
    -- Part 2
    let enumIds = parseTimes2 $ (contentlines !! 1)
    putStrLn $ show $ fst $ crt $ map (first (0-)) enumIds
