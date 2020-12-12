import ReadFile
import Data.Array
import Data.Function
import Data.Tuple.Extra

data Instruction = E Int | N Int | W Int | S Int | L Int | R Int | F Int
    deriving (Eq, Show)
-- Cartesian plane position
-- type Pos = Tuple2 Int
type Pos = (Int, Int)
-- Directions are 0 - E, 1 - N, 2 - W, 3 - S
type Direction = Int
type BoatState1 = (Direction, Pos)
-- Pair (waypoint pos, ship pos)
type BoatState2 = (Pos, Pos)

parseInstr :: String -> Instruction
parseInstr ('E':s) = E $ read s
parseInstr ('N':s) = N $ read s
parseInstr ('W':s) = W $ read s
parseInstr ('S':s) = S $ read s
parseInstr ('L':s) = L $ read s
parseInstr ('R':s) = R $ read s
parseInstr ('F':s) = F $ read s

-- Utils
addMove :: Instruction -> Pos -> Pos
addMove (E n) = first (+ n)
addMove (N n) = second (+ n)
addMove (W n) = first (subtract n)
addMove (S n) = second (subtract n)

sumPos :: Pos -> Pos -> Pos
sumPos (x, y) (x', y') = (x + x', y + y')

scalarTimes :: Int -> Pos -> Pos
scalarTimes n (x, y) = (n * x, n * y)

rotateLeft :: Int -> Pos -> Pos
rotateLeft 1 (x, y) = (-y, x)
rotateLeft 2 (x, y) = (-x, -y)
rotateLeft 3 (x, y) = (y, -x)

-- Part 1
directionToCardinal :: Direction -> (Int -> Instruction)
directionToCardinal 0 = E
directionToCardinal 1 = N
directionToCardinal 2 = W
directionToCardinal 3 = S

applyInstr1 :: Instruction -> BoatState1 -> BoatState1
applyInstr1 istr@(E _) st = second (addMove istr) st
applyInstr1 istr@(N _) st = second (addMove istr) st
applyInstr1 istr@(W _) st = second (addMove istr) st
applyInstr1 istr@(S _) st = second (addMove istr) st
applyInstr1 (L n) (dir, pos) = ((dir + n `div` 90) `mod` 4, pos)
applyInstr1 (R n) st = applyInstr1 (L $ 360 - n) st
applyInstr1 (F n) (dir, pos) = (dir, addMove (directionToCardinal dir n) pos)

-- Part 2
applyInstr2 :: Instruction -> BoatState2 -> BoatState2
applyInstr2 instr@(E _) = first $ addMove instr
applyInstr2 instr@(N _) = first $ addMove instr
applyInstr2 instr@(W _) = first $ addMove instr
applyInstr2 instr@(S _) = first $ addMove instr
applyInstr2 (L n) = first $ rotateLeft (n `div` 90)
applyInstr2 (R n) = applyInstr2 (L $ 360 - n)
applyInstr2 (F n) = \(wpos, spos) -> (wpos, sumPos spos $ scalarTimes n wpos)

distance :: Pos -> Int
distance = uncurry ((+) `on` abs)

main = do
    content <- ReadFile.readFileArg
    let instrs = map parseInstr $ lines content
    -- Part 1
    putStrLn $ show $ distance $ snd $ foldl (flip applyInstr1) (0, (0, 0)) instrs
    -- Part 2
    putStrLn $ show $ distance $ snd $ foldl (flip applyInstr2) ((10, 1), (0, 0)) instrs
