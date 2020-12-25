import ReadFile
import Data.Array
import Data.List
import Data.List.Split
import Data.Maybe

data Cell = Floor | Seat Bool
    deriving (Eq)
type Board = Array (Int, Int) Cell

instance Show Cell where
    show Floor = "."
    show (Seat True) = "#"
    show (Seat False) = "L"

parseCell :: Char -> Cell
parseCell '.' = Floor
parseCell 'L' = Seat False
parseCell '#' = Seat True

parseLine :: String -> [Cell]
parseLine = map parseCell

parseBoard :: String -> Board
parseBoard content = array bounds [((i, j), cell) | (i, row) <- zip [1..] cells,
                                                    (j, cell) <- zip [1..] row]
    where
        cells = map parseLine $ lines content :: [[Cell]]
        bounds = ((1, 1), (length cells, length $ head cells))

showBoard :: Board -> String
showBoard = unlines . chunksOf 10 . concat . fmap show

evolveCell1 :: Board -> (Int, Int) -> Cell
evolveCell1 board pos@(i, j) = updateCell cell neighbours
    where
        cell = board ! pos
        neighbours = length [ 1 | di <- [-1..1],
                                  dj <- [-1..1],
                                  inRange (bounds board) (i + di, j + dj),
                                  board ! (i + di, j + dj) == Seat True]
        updateCell :: Cell -> Int -> Cell
        updateCell Floor _ = Floor
        -- Using 5 because in the neighbours also the current cell is considered
        updateCell (Seat True) n | n >= 5    = Seat False
                                 | otherwise = Seat True
        updateCell (Seat False) n | n == 0    = Seat True
                                  | otherwise = Seat False


evolveCell2 :: Board -> (Int, Int) -> Cell
evolveCell2 board pos@(i, j) = updateCell cell neighbours
    where
        cell = board ! pos
        neighbours = sum [ firstOccupied pos di dj | di <- [-1..1],
                                                     dj <- [-1..1] ]

        firstOccupied :: (Int, Int) -> Int -> Int -> Int
        firstOccupied pos@(i, j) di dj =
            if not isWithin then
                0
            else if not isFloor then
                isFull cell
            else firstOccupied newpos di dj
            where
                newpos = (i + di, j + dj)
                isWithin = inRange (bounds board) newpos
                cell = board ! newpos
                isFloor = cell == Floor
                isFull :: Cell -> Int
                isFull (Seat True) = 1
                isFull (Seat False) = 0


        updateCell :: Cell -> Int -> Cell
        updateCell Floor _ = Floor
        -- Using 6 because in the neighbours also the current cell is considered
        updateCell (Seat True) n | n >= 6    = Seat False
                                 | otherwise = Seat True
        updateCell (Seat False) n | n == 0    = Seat True
                                  | otherwise = Seat False


evolveBoard :: (Board -> (Int, Int) -> Cell) -> Board -> Board
evolveBoard evolveCell board = array (bounds board) [(pos, evolveCell board pos) | pos <- indices board]

fixpointEvo :: (Board -> (Int, Int) -> Cell) -> Board -> Board
fixpointEvo evolveCell = fixpoint . evolutions
    where
        evolutions :: Board -> [Board]
        evolutions = iterate (evolveBoard evolveCell)
        fixpoint :: [Board] -> Board
        fixpoint = fst . fromJust . find (uncurry (==)) . (zip <*> tail)

countOccupied :: Board -> Int
countOccupied = length . filter (== Seat True) . elems

main = do
    content <- ReadFile.readFileArg
    let board = parseBoard content :: Board
    -- Part 1
    putStrLn $ show $ countOccupied $ fixpointEvo evolveCell1 $ board
    -- Part 2
    putStrLn $ show $ countOccupied $ fixpointEvo evolveCell2 $ board
