import ReadFile
import Data.Maybe
import Data.Char (isSpace)
import Data.Sort
import Data.Function
import Data.List
import Data.Tuple.Extra

-- From Wikipedia
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

type Bounds = [(Int, Int)]
type Field = (String, Bounds)
type Ticket = [Int]

parseFieldLine :: String -> Field
parseFieldLine line = (name, bounds)
    where
        (name:[rest]) = splitOn ":" line
        boundstrings = map trim $ splitOn "or" rest :: [String]
        bounds = map (\(x:[y]) -> (read x, read y)) $ map (splitOn "-") boundstrings

parseTicket :: String -> Ticket
parseTicket = map read . splitOn ","

parseInput :: String -> ([Field], [Ticket])
parseInput content = (fields, tickets)
    where
        (filedstrings:myticket:othertickets:[]) = splitOn [""] $ lines content
        fields = map parseFieldLine filedstrings
        tickets = parseTicket (last myticket):map parseTicket (tail othertickets)

checkBounds :: Int -> Bounds -> Bool
checkBounds val = foldr check False
    where
        check :: (Int, Int) -> Bool -> Bool
        check _ True = True
        check (l, u) False = l <= val && val <= u

checkTicket :: Bounds -> Ticket -> [Int]
checkTicket bounds ticket = [ v | v <- ticket, not $ checkBounds v bounds]

main = do
    content <- ReadFile.readFileArg
    let (fields, tickets) = parseInput content
    -- putStrLn $ show $ fields
    -- putStrLn $ show $ tickets
    -- Part 1
    let tickets1 = tail tickets
    let bounds = concatMap snd fields :: Bounds
    let cusu = map (checkTicket bounds) tickets1
    putStrLn $ show $ sum $ concat cusu
    -- Part 2
    let goodtickets = head tickets:(map snd $ filter (null . fst) $ zip cusu $ tail tickets)
    let possibilities = [(name, [ j | j <- [0..length fields - 1],
                                      all (\t -> checkBounds (t !! j) bounds) goodtickets
                                ]) | (name, bounds) <- fields] :: [(String, [Int])]
    let sortedposs = sortBy (compare `on` (length . snd)) possibilities
    let assoc = map (second head) $ head sortedposs:zipWith (\(_, p1) (s, p2) -> (s, p2 \\ p1)) sortedposs (tail sortedposs)
    -- putStr $ unlines $ map show $ assoc
    let indexes = map snd $ filter (isPrefixOf "departure" . fst) assoc :: [Int]
    putStrLn $ show $ product $ map (head tickets !!) indexes
