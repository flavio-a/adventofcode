import ReadFile
import Printers
import Intcode
import Data.Char (chr, ord)
import Data.List
import Data.Maybe
import Data.Array.Unboxed

type Coord = (Int, Int)
type Grid = UArray Coord Char

(|>) = flip ($)

-- Character legend
-- 35 = '#'
-- 46 = '.'
-- 10 = '\n'

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isIntersection :: Grid -> (Coord, Char) -> Bool
isIntersection grid (pos, '#') = if neighsWithin then neighsScaff else False
    where
        deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        neighs = map (addCoords pos) deltas
        neighsWithin = all (inRange $ bounds grid) neighs
        neighsScaff = all ((== '#') . (grid !)) neighs
isIntersection _ (_, _) = False

timesCoord :: Int -> Coord -> Coord
timesCoord n (x, y) = (n * x, n * y)

-- Turn left one direction
turnLeft :: Coord -> Coord
turnLeft (x, y) = (-y, x)

straightLine :: Grid -> (Coord, Coord) -> Maybe ((Char, Int), (Coord, Coord))
straightLine grid (dir, pos) = [('L', dirL), ('R', dirR)]
        |> map func
        |> filter ((> 0) . snd . fst)
        |> listToMaybe
    where
        empty :: Coord -> Bool
        empty pos = if inRange (bounds grid) pos then (grid ! pos) `elem` ".\n" else True
        -- Find the new direction
        dirL = turnLeft dir
        dirR = timesCoord (-1) dirL
        -- Maps a direction to the maximum distance distance
        distance :: Coord -> Int
        distance dir = (fromJust $ findIndex empty $ iterate (addCoords dir) pos) - 1
        -- endpos == addCoords pos $ timesCoord len dir
        func :: (Char, Coord) -> ((Char, Int), (Coord, Coord))
        func (c, newdir) = ((c, distance newdir), (newdir, addCoords pos $ timesCoord (distance newdir) newdir))

main = do
    content <- ReadFile.readFileArgDefault "inputs/day17"
    let prog = Intcode.parse content
    -- Part 1
    let vmstate = initICvm prog []
    let outputmap = execIC prog []
    let rowlen = fromJust $ findIndex (== 10) outputmap
    let dims = ((length outputmap - 2) `div` (rowlen + 1), rowlen)
    let grid = listArray ((0, 0), dims) $ map chr outputmap :: Grid
    putStrLn $ elems grid
    -- Looking for instersections
    let inters = map fst $ filter (isIntersection grid) $ assocs grid
    putStrLn $ show $ sum $ map (uncurry (*)) inters
    -- Part 2
    let newprog = prog // [(0, 2)]
    -- To do this I build the description of the path in terms of L, R and
    -- numbers and then look for three substrings such that the complete string
    -- is composition of these
    let initpos = head $ map fst $ filter ((== '^') . snd) $ assocs grid
    let straightLines = unfoldr (straightLine grid) ((-1, 0), initpos)
    putStrLn $ concat $ map (\(c, n) -> c:(show n)) $ straightLines
    -- Using this output, I found these routines by hand
    let main = "A,A,B,C,B,C,B,C,A,C"
    let routineA = "R,6,L,8,R,8"
    let routineB = "R,4,R,6,R,6,R,4,R,4"
    let routineC = "L,8,R,6,L,10,L,10"
    let input = unlines [main, routineA, routineB, routineC, "n", ""]
    putStrLn $ show $ last $ execIC newprog $ map ord input
