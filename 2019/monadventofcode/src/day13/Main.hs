import ReadFile
import Printers
import Intcode (ICProg, ICState, OutputStream, execIC, parse, (//), initICvm, execICinput)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust, fromJust, isNothing)
import Data.List (find)
import System.IO
import Control.Monad (sequence_, when)
import Control.Monad.Loops (iterateUntilM)
import Control.Concurrent

type Coord = (Int, Int)
type Grid = HashMap Coord Int

group3 :: [a] -> [((a, a), a)]
group3 (x:y:z:xs) = ((x, y), z):group3 xs
group3 [] = []
group3 _ = error "Length not multiple of three"

tileid2char :: Int -> Char
tileid2char 0 = ' '
tileid2char 1 = '█'
tileid2char 2 = '#'
tileid2char 3 = '-'
tileid2char 4 = 'o'

char2input :: Char -> Int
char2input 'k' = 0
char2input 'l' = 1
char2input 'j' = -1

-- gameStep :: ICState -> IO ICState
-- gameStep gameVM = do
--     c <- getChar
--     let (output, gameVM') = execICinput gameVM [char2input c]
--     -- "Clear" the screen
--     sequence_ [putStrLn "" | _ <- [0..30] ]
--     printGrid $ HashMap.map tileid2char $ HashMap.fromList $ group3 output
--     return gameVM'

getInput :: Grid -> Int
getInput grid =
    if cursx > ballx then -1
                    else if cursx < ballx then 1
                    else 0
    where
        (ballx, _) = head $ HashMap.keys $ HashMap.filter (== 4) grid
        (cursx, _) = head $ HashMap.keys $ HashMap.filter (== 3) grid

gameAutoStep :: (Grid, ICState) -> IO (Grid, ICState)
gameAutoStep (grid, gameVM) = do
    -- Print the grid
    printGrid $ HashMap.map tileid2char grid
    -- Moves
    let input = getInput grid
    let (output, gameVM') = execICinput gameVM [input]
    -- Take the score out of the map
    let groupoutput = group3 output
    let score = fmap snd $ find ((== (-1, 0)) . fst) groupoutput

    -- Prints the score
    sequence_ [putStrLn "" | _ <- [0..10] ]
    when (isJust score) $ putStrLn $ show $ fromJust score
    when (isNothing score) $ putStrLn ""

    let groupoutput' = filter ((/= (-1, 0)) . fst) groupoutput
    -- Update the grid
    let grid' = HashMap.union (HashMap.fromList groupoutput') grid
    threadDelay 100000
    return (grid', gameVM')

zeroBlocks :: (Grid, ICState) -> Bool
zeroBlocks = (0 ==) . length . filter (== 2) . HashMap.elems . fst

main = do
    content <- ReadFile.readFileArgDefault "inputs/day13"
    let prog = Intcode.parse content
    -- Part 1
    let ostream = execIC prog []
    let grid = HashMap.fromList $ group3 ostream
    putStrLn $ show $ length $ filter (== 2) $ HashMap.elems grid
    -- Part 2
    let prog2 = prog // [(0, 2)]
    let (output, gameVM) = execICinput (initICvm prog2 []) []
    let grid = HashMap.fromList $ group3 output

    -- Run the game
    iterateUntilM zeroBlocks gameAutoStep (grid, gameVM)
