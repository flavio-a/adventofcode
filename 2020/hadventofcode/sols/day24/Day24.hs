import ReadFile
import Data.List

data Step = E | W | NE | NW | SE | SW
    deriving (Eq, Show)
-- The first coord is the row. A rows' 0 is to the NE of the 0 of the row
-- right below
type TilePos = (Int, Int)

parseLine :: String -> [Step]
parseLine [] = []
parseLine ('e':str) = E:(parseLine str)
parseLine ('w':str) = W:(parseLine str)
parseLine ('n':'e':str) = NE:(parseLine str)
parseLine ('n':'w':str) = NW:(parseLine str)
parseLine ('s':'e':str) = SE:(parseLine str)
parseLine ('s':'w':str) = SW:(parseLine str)

addStep :: TilePos -> Step -> TilePos
addStep (r, c) E = (r, c + 1)
addStep (r, c) W = (r, c - 1)
addStep (r, c) NE = (r + 1, c)
addStep (r, c) NW = (r + 1, c - 1)
addStep (r, c) SE = (r - 1, c + 1)
addStep (r, c) SW = (r - 1, c)

stepsToCoord :: [Step] -> TilePos
stepsToCoord = foldl' addStep (0, 0)

dropEqualPairs :: (Eq a) => [a] -> [a]
dropEqualPairs (x:y:xs) | x == y    = dropEqualPairs xs
                        | otherwise = x:(dropEqualPairs (y:xs))
dropEqualPairs l = l

becomesBlack :: TilePos -> [TilePos] -> Bool
becomesBlack (r, c) tiles = (isBlack && not flipBlack) || ((not isBlack) && flipWhite)
    where
        neighbours = length [ 1 | (dx, dy) <- [(0, 1), (0, -1), (1, -1), (1, 0), (-1, 0), (-1, 1)],
                                  (r + dx, c + dy) `elem` tiles
                            ]
        isBlack = (r, c) `elem` tiles
        flipBlack = neighbours == 0 || neighbours > 2
        flipWhite = neighbours == 2

boardStep :: [TilePos] -> [TilePos]
boardStep tiles = [ (r, c) | r <- [fst boundsr - 1 .. snd boundsr + 1],
                             c <- [fst boundsc - 1 .. snd boundsc + 1],
                             becomesBlack (r, c) tiles
                  ]
    where
        boundsr = (foldl1' min $ map fst tiles, foldl1' max $ map fst tiles)
        boundsc = (foldl1' min $ map snd tiles, foldl1' max $ map snd tiles)

main = do
    content <- ReadFile.readFileArg
    let coords = dropEqualPairs $ sort $ map (stepsToCoord . parseLine) $ lines content
    -- Part 1
    putStrLn $ show $ length $ coords
    -- Part 2
    putStrLn $ show $ length $ iterate boardStep coords !! 100
