module Printers (
    printGrid,
) where

import Data.Foldable (for_)
import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
type Grid = HashMap Coord Char

printGrid :: Grid -> IO ()
printGrid grid = do
    let tmp = HashMap.keys grid
    let minx = minimum $ map fst tmp
    let maxx = maximum $ map fst tmp
    let miny = minimum $ map snd tmp
    let maxy = maximum $ map snd tmp
    for_ [(i, j) | j <- [maxy,maxy-1..miny], i <- [minx..maxx] ] (\(x, y) -> do
            putChar $ fromMaybe ' ' $ HashMap.lookup (x, y) grid
            when (x == maxx) $ putChar '\n'
        )
