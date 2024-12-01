import ReadFile
-- import Printers
import Intcode
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.State.Lazy

type Coord = (Int, Int)
type Grid = Map Coord Int

-- tileid2char :: Int -> Char
-- tileid2char 0 = '█'
-- tileid2char 1 = ' '
-- tileid2char 2 = 'o'

move :: Int -> Coord -> Coord
move 1 (x, y) = (x, y + 1)
move 2 (x, y) = (x, y - 1)
move 3 (x, y) = (x + 1, y)
move 4 (x, y) = (x - 1, y)

max_depth = 1000 :: Int

-- This isn't the best idea ever: I'm using a DFS to compute distances...
-- Well, a star is a star

-- Filling the map with a DFS (monadic to have global state). Return the
-- minimum distance to the target
dfsM :: Int -> Coord -> ICState -> State Grid Int
dfsM count pos vmstate | count >= max_depth = error "Too deep"
                       | otherwise          = do
    tmp <- forM [1..4] moveDir
    tmp' <- mapM cusu $ catMaybes tmp
    return $ minimum $ (max_depth + 1):tmp'
        where
            moveDir :: Int -> State Grid (Maybe (Either (Coord, ICState) Int))
            moveDir dir = do
                let newpos = move dir pos
                grid <- get
                if Map.member newpos grid then return Nothing else do
                    let ([out], newvmstate) = execICinput vmstate [dir]
                    put $ Map.insert newpos out grid
                    case out of
                        0 -> return Nothing
                        1 -> return $ Just $ Left (newpos, newvmstate)
                        2 -> return $ Just $ Right $ count + 1
            cusu :: Either (Coord, ICState) Int -> State Grid Int
            cusu (Right n) = return n
            cusu (Left (pos, vmstate)) = dfsM (count + 1) pos vmstate

-- Getting the maximum distance one the grid already built. The state simply
-- tells whether the cell has already been visited
dfs2M :: Grid -> Int -> Coord -> State Grid ()
dfs2M globalmap count pos = do
    modify $ Map.insert pos count
    tmp <- forM [1..4] moveDir
    mapM_ (dfs2M globalmap (count + 1)) $ catMaybes tmp
        where
            moveDir :: Int -> State Grid (Maybe Coord)
            moveDir dir = do
                let newpos = move dir pos
                visited <- fmap (Map.lookup newpos) get
                if isJust visited then return Nothing else
                    case Map.lookup newpos globalmap of
                        Just 0 -> return Nothing
                        Just 1 -> return $ Just newpos
                        Just 2 -> error "This shouldn't happen"

main = do
    content <- ReadFile.readFileArgDefault "inputs/day15"
    let prog = Intcode.parse content
    -- Part 1
    let vmstate = initICvm prog []
    let (dist, map) = runState (dfsM 0 (0, 0) vmstate) $ Map.singleton (0, 0) 1
    putStrLn $ show $ dist
    -- printGrid $ Map.map tileid2char map
    -- Part 2
    let oxygenpos = fst $ head $ filter ((== 2) . snd) $ Map.toList map
    let distsfromoxygen = execState (dfs2M map 0 oxygenpos) $ Map.empty
    putStrLn $ show $ maximum $ Map.elems distsfromoxygen
