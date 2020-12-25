import ReadFile
import Data.List
import Data.Maybe

type Card = Int
type Deck = [Int]
type DeckPair = (Deck, Deck)

parseInput :: String -> DeckPair
parseInput str = (d1, d2)
    where
        d1:d2:_ = map (map read . tail) $ splitOn [""] $ lines str

-- Expects both deck not to be empty
doRound1 :: DeckPair -> DeckPair
doRound1 (d1, d2) = (d1', d2')
    where
        c1 = head d1
        c2 = head d2
        d1' = tail d1 ++ if c1 > c2 then [c1, c2] else []
        d2' = tail d2 ++ if c1 > c2 then [] else [c2, c1]


firstWins :: DeckPair -> Bool
firstWins (d1, d2) = if null d1 then False else True

-- Expects both deck not to be empty
doRound2 :: DeckPair -> DeckPair
doRound2 (d1, d2) = (d1', d2')
    where
        c1 = head d1
        c2 = head d2
        recurse = c1 < length d1 && c2 < length d2
        recursiveRound = (take c1 $ tail d1, take c2 $ tail d2) :: DeckPair
        winner = if recurse then
                firstWins $ getEnd doRound2 recursiveRound
            else
                c1 > c2

        d1' = tail d1 ++ if winner then [c1, c2] else []
        d2' = tail d2 ++ if winner then [] else [c2, c1]

getEnd :: (DeckPair -> DeckPair) -> DeckPair -> DeckPair
getEnd doRound decks = end
    where
        rounds = iterate doRound decks :: [DeckPair]
        end = fst $ fromJust $ find isEnded $ zipInits $ rounds

        zipInits :: [a] -> [(a, [a])]
        -- zipInits = zip <*> inits
        zipInits [] = []
        zipInits list = scanl buildHist (head list, []) (tail list)
            where
                buildHist (x, xs) y = (y, x:xs)

        isEnded :: (DeckPair, [DeckPair]) -> Bool
        isEnded (pair@(d1, d2), hist) = null d1 || null d2 || pair `elem` hist


getWinnerScore :: DeckPair -> Int
getWinnerScore pair = sum $ zipWith (*) (reverse winner) [1..]
    where
        winner = if firstWins pair then fst pair else snd pair

main = do
    content <- ReadFile.readFileArg
    let decks = parseInput content
    -- Part1
    putStrLn $ show $ getWinnerScore $ getEnd doRound1 decks
    -- Part 2
    putStrLn $ show $ getWinnerScore $ getEnd doRound2 decks
