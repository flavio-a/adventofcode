import ReadFile
import Printers
import Intcode
import Control.Monad
import Data.List (find)
import Data.Maybe (fromJust)

minmax :: Ord a => [a] -> (a, a)
minmax l = (minimum l, maximum l)

isInRange :: ICProg -> Int -> Int -> Bool
isInRange prog x y = execIC prog [x, y] == [1]

minimumy :: RealFrac a => ICProg -> a -> Int -> Int
minimumy prog m x = fromJust $ find (isInRange prog x) [esty..]
    where
        -- Conservative estimate of of y
        esty = fromIntegral $ floor $ 0.95 * m * (fromIntegral x)

squaresize = 100 :: Int

squareFits :: RealFrac a => ICProg -> a -> Int -> Bool
squareFits prog m x = isInRange prog (x - squaresize + 1) bottomlefty
    where
        -- Minimum y such that (x, y) is within the tractor beam
        toprighty = minimumy prog m x
        bottomlefty = toprighty + squaresize - 1

main = do
    content <- ReadFile.readFileArgDefault "inputs/day19"
    let prog = Intcode.parse content
    -- Part 1
    let firstScan = [ (x, y) | x <- [0..49],
                               y <- [0..49],
                               isInRange prog x y
                               ]
    putStrLn $ show $ length firstScan
    -- Part 2
    -- putStrLn $ unlines $ [ [if isInRange prog x y then '#' else '.' | x <- [0..49]] | y <- [0..49]]
    -- This assumes that both ms are >= 1, so that both intersections with the
    -- 50x50 square are on the last line (y = 50). If this isn't the case,
    -- print error
    let (minx, maxx) = minmax $ map fst $ filter ((== 49) . snd) firstScan
    when (maxx == 49) $ error "the maximum x is supposed to be lower than 49"
    let m1 = 49 / fromIntegral maxx
    let m2 = 49 / fromIntegral minx

    -- Fixed an x, we put the 100x100 square with its top right corner at (x,
    -- m1 * x). Thus we have the top left corner at (x - 100, m1 * x), and so
    -- the bottom left corner at (x - 100, m1 * x + 100). So we need m1 * x +
    -- 100 <= m2 * (x - 100). This gives an estimate of the right x, that is
    -- 100 + m2 * 100 <= (m2 - m1) * x
    let estimatedx = fromIntegral squaresize * (1 + m2) / (m2 - m1)
    -- Just to be sure starts checking a few x earlier
    let startx = floor $ estimatedx * 0.95
    let targetx = fromJust $ find (squareFits prog m1) [startx..]
    when (targetx == startx) $ error "The first x already satisfies: try reducing the factor of startx"
    let starty = fromIntegral $ floor $ 0.95 * m1 * (fromIntegral targetx)
    let targety = fromJust $ find (isInRange prog targetx) [starty..]
    -- putStrLn $ show $ (targetx, targety)
    putStrLn $ show $ (targetx - squaresize + 1) * 10000 + targety
