import ReadFile
import Printers
import Intcode (ICProg, execIC, parse)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
-- Cell 0 = black, 1 = white
type Grid = HashMap Coord Int
-- This should always be (+-1, 0) or (0, +-1)
type Direction = (Int, Int)

-- The first argument can be either 0 (rotate left) or 1 (rotate right)
rotate :: Int -> Direction -> Direction
rotate 0 (x, y) = (-y, x)
rotate 1 (x, y) = (y, -x)

move :: Direction -> Coord -> Coord
move (dx, dy) (x, y) = (x + dx, y + dy)

type WorldState = (Grid, Coord, Direction)

pairList :: [a] -> [(a,a)]
pairList (x0:x1:xs) = (x0,x1) : pairList xs
pairList [] = []
pairList _ = error "odd number of elements"

readWorld :: WorldState -> Int
readWorld (grid, pos, _) = fromMaybe 0 $ grid HashMap.!? pos

step :: (Int, Int) -> WorldState -> WorldState
step (newcol, turn) (grid, pos, dir) = (HashMap.insert pos newcol grid, move dir' pos, dir')
    where
        dir' = rotate turn dir

evolve :: ICProg -> WorldState -> [WorldState]
evolve prog initworld = worlds
    where
        iofun :: [Int] -> [Int]
        iofun = execIC prog
        -- Bind together the world and the robot
        worlds :: [WorldState]
        worlds = initworld:(map (uncurry step) $ zip ostream worlds)
        istream = map readWorld worlds
        ostream = pairList $ iofun istream

main = do
    content <- ReadFile.readFileArgDefault "inputs/day11"
    let prog = Intcode.parse content
    -- putStrLn $ show $ pairList $ execIC prog [0..]
    -- Part 1
    let initworld = (HashMap.empty, (0, 0), (0, 1))
    let (endgrid, _, _) = last $ evolve prog initworld
    putStrLn $ show $ HashMap.size endgrid
    -- Part 2
    let initworld = (HashMap.singleton (0, 0) 1, (0, 0), (0, 1))
    let (endgrid, _, _) = last $ evolve prog initworld
    printGrid $ HashMap.map (\color -> if color == 0 then ' ' else '█') endgrid
