import ReadFile
import Data.List
import Data.Array
import Data.Tuple
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Ord (Ordering, compare)
import Data.Tuple.Extra (second)
import qualified Data.IntMap.Strict as Map

-- From Haskell Wiki
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

zipWithA :: (Ix i) => (a -> b -> c) -> Array i a -> Array i b -> Array i c
zipWithA f xs ys = listArray (bounds xs) [f (xs ! i) (ys ! i) | i <- range (bounds xs)]

type Grid = [[Bool]]
type Tile = (Int, Grid)

boolToCell :: Bool -> Char
boolToCell True  = '#'
boolToCell False = '.'

cellToBool :: Char -> Bool
cellToBool '#' = True
cellToBool '.' = False
cellToBool ' ' = False

showGrid :: Grid -> String
showGrid = unlines . map (map boolToCell)

showTile :: Tile -> String
showTile tile = "ID: " ++ (show $ fst tile) ++ "\n" ++ showGrid (snd tile)

parseTile :: [String] -> Tile
parseTile strs = (tid, cont)
    where
        tid = read $ drop 5 $ init $ head strs
        cont = map (map cellToBool) $ tail strs

type Borders = [Int]

getBorders :: Grid -> Borders
-- getBorders tile = (toNum top, toNum right, toNum bot, toNum left)
getBorders cont = map toNum ([top, right, bot, left] ++ map reverse [top, right, bot, left])
    where
        top = head cont
        right = map last cont
        bot = reverse $ last cont
        left = reverse $ map head cont

        toNum :: [Bool] -> Int
        toNum = foldl' folder 0
            where
                folder tot False = 2 * tot
                folder tot True = 2 * tot + 1

-- Returns neighbours
neighbours :: [(Int, Int)] -> [(Int, [Int])]
neighbours pairs = res
    where
        pairs' = sort $ concatMap (\x -> [x, swap x]) pairs
        res = map (\xs -> (fst $ head xs, nub $ map snd xs)) $ groupBy ((==) `on` fst) pairs'

type TileMap a = Map.IntMap a
-- Adds a diagonal stratum to the final grid of tiles
addStratum :: TileMap [Int] -> [Int] -> [Int] -> [Int]
-- addStratum neighMap oldStratum currStratum = newStratum
addStratum neighMap oldStratum currStratum = newStratum
    where
        getNeigh :: Int -> [Int]
        getNeigh = fromJust . flip Map.lookup neighMap

        tmp = map (uncurry intersect) $ zip <*> tail $ map getNeigh currStratum :: [[Int]]
        newStratum' = map (\\ oldStratum) tmp
        newFirst = getNeigh (head currStratum) \\ ([head $ head newStratum'] ++ oldStratum)
        newLast = getNeigh (last currStratum) \\ ([head $ last newStratum'] ++ oldStratum)
        newStratum = newFirst ++ map head newStratum' ++ newLast

-- Pair (angle (0..3), fliph (+-1), flipv (+-1))
type Rotation = (Int, Int, Int)
snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

sharedBorder :: Grid -> Grid -> Int
sharedBorder grid1 grid2 = head $ intersect (getBorders grid1) (getBorders grid2)

rotBorderIdx :: Int -> Rotation -> Int
rotBorderIdx idx (rot, 1, 1) = (idx + rot) `rem` 4
rotBorderIdx idx (rot, -1, -1) = (idx + rot + 2) `rem` 4
rotBorderIdx idx (rot, -1, 1) = (10 - ((idx + rot) `rem` 4)) `rem` 4 + 4
rotBorderIdx idx (rot, 1, -1) = (8 - ((idx + rot) `rem` 4)) `rem` 4 + 4

-- Given the tile to the left and its rotation, compute the rotation of the
-- current tile (with flippedv always 1 because it's used before calcRotT)
calcRotL :: Grid -> Rotation -> Grid -> Rotation
calcRotL oldgrid oldRot grid = (rot, flippedh, 1)
    where
        shbrd = sharedBorder oldgrid grid :: Int
        oldidx = rotBorderIdx (fromJust $ shbrd `elemIndex` getBorders oldgrid) oldRot
        idx = fromJust $ shbrd `elemIndex` getBorders grid
        -- Put the shared border to the left (index 3)
        rot = (7 - idx) `rem` 4
        idx' = (idx + rot) `rem` 4 + (idx `div` 4) * 4
        -- Flip if the borders have the same direction
        flippedh = if (oldidx == 5 && idx' == 7) || (oldidx == 1 && idx' == 3) then -1 else 1

-- Given the tile above and its rotation, compute the rotation of the
-- current tile
calcRotT :: Grid -> Rotation -> Grid -> Rotation
calcRotT oldgrid oldRot grid = (rot, 1, flippedv)
    where
        shbrd = sharedBorder oldgrid grid :: Int
        oldidx = rotBorderIdx (fromJust $ shbrd `elemIndex` getBorders oldgrid) oldRot
        idx = fromJust $ shbrd `elemIndex` getBorders grid
        -- Put the shared border above (index 0)
        rot = (8 - idx) `rem` 4
        idx' = (idx + rot) `rem` 4 + (idx `div` 4) * 4
        -- Flip if the borders have the same direction
        flippedv = if (oldidx == 6 && idx' == 4) || (oldidx == 2 && idx' == 0) then -1 else 1

applyRot :: Rotation -> Grid -> Grid
applyRot (r, h, v) = flipv v . fliph h . rotate r
    where
        flipv 1    = id
        flipv (-1) = map reverse

        fliph 1    = id
        fliph (-1) = reverse

        rotate 0 = id
        rotate 2 = reverse . map reverse
        rotate 3 = rotate 1 . rotate 2
        rotate 1 = \g -> [ reverse $ map (!! i) g | i <- [0..length g - 1] ]

showGridsArray :: Array (Int, Int) Grid -> String
showGridsArray tiles = unlines [ row i | i <- [0..squaresize * 10 - 1]]
    where
        squaresize = fst $ snd $ bounds tiles

        row :: Int -> String
        row i = (unwords $ map (map boolToCell) [ (tiles ! ((i `div` 10) + 1, j)) !! (i `rem` 10) | j <- [1..squaresize] ])
             ++ (if i `rem` 10 == 9 then "\n" else "")

stripBorder :: Grid -> Grid
stripBorder = tail . init . map (tail . init)

concatGrid :: Array (Int, Int) Grid -> Grid
concatGrid grid = [ row i | i <- [0..squaresize * 8 - 1]]
    where
        squaresize = fst $ snd $ bounds grid

        row :: Int -> [Bool]
        row i = concat [ (grid ! ((i `div` 8) + 1, j)) !! (i `rem` 8) | j <- [1..squaresize] ]

monster = map (map cellToBool) $ lines "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   " :: Grid
monsterI = 3
monsterJ = 20
searchMonster :: Grid -> Int -> Int -> Bool
searchMonster grid i j = and [ not (monster !! i' !! j') || (grid !! (i + i') !! (j + j')) | i' <- [0..monsterI - 1], j' <- [0..monsterJ - 1] ]

searchAllMonster :: Grid -> [(Int, Int)]
searchAllMonster grid = [ (i, j) | i <- [0..length grid - monsterI], j <- [0..length (head grid) - monsterJ], searchMonster grid i j ]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

isInMonsters :: [(Int, Int)] -> Int -> Int -> Bool
isInMonsters origs i j = or [ monster !! (i - x) !! (j - y) | (x, y) <- origs,
                                                              i - x < monsterI, 0 <= i - x,
                                                              j - y < monsterJ, 0 <= j - y ]

main = do
    content <- ReadFile.readFileArg
    let tiles = map parseTile $ splitOn [""] $ lines content :: [Tile]
    let allBorders = [ (border, fst tile) | tile <- tiles,
                                            border <- getBorders (snd tile)
                                          ]
    let neighbourTilesPairs = map (uncurry ((,) `on` snd))
                            $ filter (uncurry ((==) `on` fst))
                            $ zip <*> tail
                            $ sort allBorders
    let neightboursList = neighbours neighbourTilesPairs
    let neighMap = Map.fromList neightboursList :: TileMap [Int]
    let corners = map fst $ filter ((==) 2 . length . snd) $ neightboursList
    -- Part 1
    putStrLn $ show $ product $ corners
    -- Part 2
    let squaresize = squareRoot $ length tiles :: Int
    let cornerTL = head corners
    let firstStratums = [[cornerTL], fromJust $ Map.lookup cornerTL neighMap] :: [[Int]]
    let stratums = take (2 * squaresize - 1)
                 $ (firstStratums ++ (zipWith (addStratum neighMap) stratums $ tail stratums))
    let tileGrid = array ((1, 1), (squaresize, squaresize))
                 $ [ (pos, tile) | (diag, stratum) <- zip [1..] stratums,
                                   (x, tid) <- zip [1 + max (diag - squaresize) 0..] stratum,
                                   let pos = (x, diag - x + 1),
                                   let tile = (tid, fromJust $ lookup tid tiles)
                   ] :: Array (Int, Int) Tile
    let getGrid = snd . (tileGrid !) :: (Int, Int) -> Grid
    let _shrdBrd = sharedBorder `on` (getGrid) :: (Int, Int) -> (Int, Int) -> Int

    let firstRot = (5 - fromJust ((_shrdBrd (1, 1) (1, 2)) `elemIndex` getBorders (getGrid (1, 1)))) `rem` 4 :: Int
    let temp = fromJust $ _shrdBrd (1, 1) (2, 1) `elemIndex` getBorders (getGrid (1, 1))
    let firstRfl = case (temp + firstRot) `rem` 4 of 0 -> (-1)
    let flRots = [((1, 1), (firstRot, firstRfl, 1))] ++ zipWith (\j (_, oldRot) -> ((1, j), calcRotL (getGrid (1, j - 1)) oldRot (getGrid (1, j)) )) [2..squaresize] flRots :: [((Int, Int), Rotation)]
    let cusu = (\i -> map (\((_, j), oldRot) -> ((i, j), calcRotT (getGrid (i - 1, j)) oldRot (getGrid (i, j))))) :: Int -> [((Int, Int), Rotation)] -> [((Int, Int), Rotation)]
    let _rots = [flRots] ++ (zipWith cusu [2..squaresize] _rots) :: [[((Int, Int), Rotation)]]
    let rots = array (bounds tileGrid) $ concat _rots
    let rotTiles = zipWithA applyRot rots $ fmap snd tileGrid
    -- putStrLn $ showGridsArray rotTiles
    let finalGrid = concatGrid $ fmap stripBorder rotTiles
    -- This finds the right rotation
    -- putStrLn $ show $ [ (r, h, v) | r <- [0..3], h <-[1, -1], v <- [1, -1], 0 < (length $ searchAllMonster $ applyRot (r, h, v) finalGrid)]
    let (rotMyInput, monsters) = head [ ((r, h, v), msts) | r <- [0..3], h <-[1, -1], v <- [1, -1],
                                                            let msts = searchAllMonster $ applyRot (r, h, v) finalGrid,
                                                            0 < length msts]
    -- putStrLn $ showGrid $ applyRot rotMyInput finalGrid
    -- putStrLn $ show $ monsters
    -- putStrLn $ show $ sum [ 1 | i <- [0..length finalGrid - 1], j <- [0..length finalGrid - 1], finalGrid !! i !! j ] - (15 * length monsters)
    putStrLn $ show $ sum [ 1 | i <- [0..length finalGrid - 1], j <- [0..length finalGrid - 1],
                                (applyRot rotMyInput finalGrid) !! i !! j,
                                not $ isInMonsters monsters i j ]
