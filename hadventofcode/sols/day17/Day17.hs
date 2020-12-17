import ReadFile
import Data.Array

type Cell = Bool

parseCell :: Char -> Cell
parseCell '#' = True
parseCell '.' = False

next :: Cell -> Int -> Cell
-- In the sum is also considered the center
next True n | n == 3 || n == 4 = True
            | otherwise        = False
next False n | n == 3    = True
             | otherwise = False

sumBoard :: (Foldable t, Functor t) => t Cell -> Int
sumBoard = sum . fmap fromEnum
--     where
--     fromEnum == cellToInt
--         cellToInt True = 1
--         cellToInt False = 0

type Coord = (Int, Int, Int)
type Board = Array Coord Cell

parseGrid :: String -> Board
parseGrid content = listArray ((1, 1, 0), (len, len, 0)) $ concat cusu
    where
        cusu = map (map parseCell) $ lines content
        len = length cusu

sumCoord :: Coord -> Coord -> Coord
sumCoord (x, y, z) (x', y', z') = (x + x', y + y', z + z')

updateCell :: Board -> Coord -> Cell
updateCell board pos@(x, y, z) = next current neighbours
    where
        current = inRange (bounds board) pos && (board ! pos)

        neighbours = sum [ 1 | dx <- [-1..1],
                               dy <- [-1..1],
                               dz <- [-1..1],
                               let npos = sumCoord pos (dx, dy, dz),
                               inRange (bounds board) npos,
                               board ! npos
                             ]

updateBoard :: Board -> Board
updateBoard board = array ((x1 - 1, y1 - 1, z1 - 1), (x2 + 1, y2 + 1, z2 + 1)) cusu
    where
        ((x1, y1, z1), (x2, y2, z2)) = bounds board
        cusu = [((x, y, z), updateCell board (x, y, z))
               | x <- [x1 - 1..x2 + 1],
                 y <- [y1 - 1..y2 + 1],
                 z <- [z1 - 1..z2 + 1]
               ]

showBoardPlane :: Board -> Int -> String
showBoardPlane board z = unlines $ map (map showCell) cusu
    where
        ((x1, y1, _), (x2, y2, _)) = bounds board
        cusu = [[board ! (x, y, z) | y <- [y1..y2]] | x <- [x1..x2]]

        showCell True = '#'
        showCell False = '.'


type Coord2 = (Int, Int, Int, Int)
type Board2 = Array Coord2 Cell

parseGrid2 :: String -> Board2
parseGrid2 content = listArray ((1, 1, 0, 0), (len, len, 0, 0)) $ concat cusu
    where
        cusu = map (map parseCell) $ lines content
        len = length cusu

sumCoord2 :: Coord2 -> Coord2 -> Coord2
sumCoord2 (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

updateCell2 :: Board2 -> Coord2 -> Cell
updateCell2 board pos@(x, y, z, w) = next current neighbours
    where
        current = inRange (bounds board) pos && (board ! pos)

        neighbours = sum [ 1 | dx <- [-1..1],
                               dy <- [-1..1],
                               dz <- [-1..1],
                               dw <- [-1..1],
                               let npos = sumCoord2 pos (dx, dy, dz, dw),
                               inRange (bounds board) npos,
                               board ! npos
                             ]

updateBoard2 :: Board2 -> Board2
updateBoard2 board = array ((x1 - 1, y1 - 1, z1 - 1, w1 - 1), (x2 + 1, y2 + 1, z2 + 1, w2 + 1)) cusu
    where
        ((x1, y1, z1, w1), (x2, y2, z2, w2)) = bounds board
        cusu = [((x, y, z, w), updateCell2 board (x, y, z, w))
               | x <- [x1 - 1..x2 + 1],
                 y <- [y1 - 1..y2 + 1],
                 z <- [z1 - 1..z2 + 1],
                 w <- [w1 - 1..w2 + 1]
               ]

main = do
    content <- ReadFile.readFileArg
    -- Part 1
    let initialBoard = parseGrid content
    let board6 = iterate updateBoard initialBoard !! 6
    putStrLn $ show $ sumBoard board6
    -- Part 2
    let initialBoard2 = parseGrid2 content
    let board62 = iterate updateBoard2 initialBoard2 !! 6
    putStrLn $ show $ sumBoard board62
