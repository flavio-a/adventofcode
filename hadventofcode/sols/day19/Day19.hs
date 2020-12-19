import ReadFile
import Data.Array
import Data.List (isPrefixOf, nub, sort, splitAt)
import Data.String.Utils (strip)

import Debug.Trace

type RHS = [Int]
data Prod = Const Char | Rules [RHS]
    deriving (Eq, Show)
type Grammar = Array Int Prod

parseLine :: String -> (Int, Prod)
parseLine line = (idx, prods)
    where
        (idx':rest) = map strip $ splitOneOf ":|" line
        idx = read idx'

        prods = if length rest == 1 && "\"" `isPrefixOf` head rest
            then
                Const $ (head rest !! 1)
            else
                Rules $ map (map read . words) rest

parseGrammar :: [String] -> Grammar
parseGrammar prods = array (0, length grammarlist - 1) grammarlist
    where
        grammarlist = map parseLine prods

-- -- Get some info on the grammar:
-- --   the first value is the length of strings matched by that category
-- --   the second value is the list of possible first character for that category
-- getInfo :: Grammar -> Array Int (Int, [Char])
-- getInfo grammar = info
--     where
--         info = array (bounds grammar) $
--             [ (idx, go rules) | (idx, rules) <- assocs grammar ]
--
--         go :: Prod -> (Int, [Char])
--         go (Const c) = (1, [c])
--         go (Rules rules) = (len, fcs)
--             where
--                 lens = map sum $ [ [ fst (info ! i) | i <- rule] | rule <- rules ]
--                 len = if all (== head lens) lens
--                     then
--                         head lens
--                     else
--                         trace "ERROR: different lengths" $ head lens
--                 fcs = sort $ nub $ concat $ [ snd (info ! head rule) | rule <- rules]

-- Get the length of strings matched by all categories
getLengths :: Grammar -> Array Int Int
getLengths grammar = lens
    where
        lens = array (bounds grammar) $
            [ (idx, go rules) | (idx, rules) <- assocs grammar ]

        go :: Prod -> Int
        go (Const c) = 1
        go (Rules rules) = len
            where
                _lens = map sum $ [ [ lens ! i | i <- rule] | rule <- rules ]
                len = if all (== head _lens) (tail _lens)
                    then
                        head _lens
                    else
                        trace "ERROR: different lengths" $ head _lens

splitAts :: [Int] -> String -> [String]
splitAts [] s = [s]
splitAts (n:ns) s = h:splitAts ns rest
    where
        (h, rest) = splitAt n s

-- Check whether a rule matches a string or not assuming that the
-- string has the right length (rl)
rlMatchRule :: (Int -> String -> Bool) -> RHS -> Array Int Int -> String -> Bool
rlMatchRule matchCat rule lens = all (uncurry matchCat) . zip rule . splitAts (map (lens !) rule)

matchGrammar1 :: Grammar -> String -> Bool
matchGrammar1 grammar = matchCategory 0
    where
        lens = getLengths grammar

        matchCategory :: Int -> String -> Bool
        matchCategory idx str = case grammar ! idx of
            Const c     -> str == [c]
            Rules rules -> any id [ matchRule idx rule str | rule <- rules]

        matchRule :: Int -> [Int] -> String -> Bool
        matchRule idx rule str = isRightLength && doesMatch
            where
                isRightLength = length str == (lens ! idx)
                doesMatch = rlMatchRule matchCategory rule lens str

prodContain :: Int -> Prod -> Bool
prodContain _ (Const _) = False
prodContain n (Rules rules) = any (n `elem`) rules

-- This uses different 8 and 11 rule. Moreover it assumes 0 <- 8 11 and that
-- only 0 uses 8 and 11
matchGrammar2 :: Grammar -> String -> Bool
matchGrammar2 grammar = matchCategory 0
    where
        lens = getLengths grammar

        matchCategory :: Int -> String -> Bool
        matchCategory 0 str = matchRule0811 0 str
        matchCategory 8 str = matchRule0811 8 str
        matchCategory 11 str = matchRule0811 11 str
        matchCategory idx str = case grammar ! idx of
            Const c     -> str == [c]
            Rules rules -> any id [ matchRule idx rule str | rule <- rules]

        matchRule0811 :: Int -> String -> Bool
        matchRule0811 0 str = any match811 splits
            where
                splits = [ splitAt i str | i <- [1..length str - 1]]
                match811 (s1, s2) = matchRule0811 8 s1 && matchRule0811 11 s2
        matchRule0811 8 str = isRightLength && matchRule 8 rule str
            where
                len42 = lens ! 42
                isRightLength = length str `rem` len42 == 0
                rep = length str `div` len42
                rule = replicate rep 42
        matchRule0811 11 str = isRightLength && matchRule 11 rule str
            where
                len4231 = (lens ! 42) + (lens ! 31)
                isRightLength = length str `rem` len4231 == 0
                rep = length str `div` len4231
                rule = replicate rep 42 ++ replicate rep 31

        matchRule :: Int -> [Int] -> String -> Bool
        matchRule idx rule str = isRightLength && doesMatch
            where
                isRightLength = (idx `elem` [0, 8, 11])
                             || (length str == (lens ! idx))
                doesMatch = rlMatchRule matchCategory rule lens str


main = do
    content <- ReadFile.readFileArg
    let grammarstring:strings:[] = splitOn [""] $ lines content
    let grammar = parseGrammar $ grammarstring
    -- Part 1
    let matching = filter (matchGrammar1 grammar) strings
    putStrLn $ show $ length matching
    -- Part 2
    -- This confirms that 8 and 11 are used only by 0, so I can handle them
    -- in a special way
    putStrLn $ show $ filter (prodContain 8 . snd) $ assocs grammar
    putStrLn $ show $ filter (prodContain 11 . snd) $ assocs grammar
    putStrLn $ show $ length $ filter (matchGrammar2 grammar) strings
