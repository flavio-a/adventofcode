module Intcode (
    parse,
    showICProg,
    execIC,
    execICfull,
    (//),
    (!)
    )
where

import Data.List (intercalate, unfoldr)
import Data.List.Split (splitOn, splitOneOf)
import Data.Array.Unboxed
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe (catMaybes)
import Control.Monad.State.Lazy
-- import Control.Monad.Extra
import Control.Monad.Loops (untilM')

type OPCode = Int
type IP = Int
type Addr = Int
type ICProg = UArray Addr OPCode
type Input = Int
type InputStream = [Input]
type Output = Int
type OutputStream = [Output]
type Mode = Int

parse :: String -> ICProg
parse str = listArray (0, length list - 1) list
    where
        list = map read $ splitOn "," str

showICProg :: ICProg -> String
showICProg = intercalate "," . fmap show . elems

-- I would like a monad in which execute instructions
-- The monad is State, where my state is a tuple (IP, memory, inputstream)
type ICState = (IP, ICProg, InputStream)

getIP :: State ICState IP
getIP = get >>= (return . fst3)

setIP :: IP -> State ICState ()
setIP newip = modify (\(_, mem, istream) -> (newip, mem, istream))

incrIP :: Int -> State ICState ()
incrIP d = modify (\(oldip, mem, istream) -> (oldip + d, mem, istream))

getMem :: Addr -> State ICState OPCode
getMem addr = get >>= (return . (! addr) . snd3)

setMem :: Addr -> OPCode -> State ICState ()
setMem addr newop = modify (\(ip, oldmem, istream) -> (ip, oldmem // [(addr, newop)], istream))

getInput :: State ICState Input
getInput = do
    (ip, mem, istream) <- get
    put (ip, mem, tail istream)
    return $ head istream

-- Get the current instruction
getCurrInstr :: State ICState OPCode
getCurrInstr = getIP >>= getMem

-- Get values in memory at the current position plus an offset
getCurrOffset :: Int -> State ICState OPCode
getCurrOffset d = getIP >>= (getMem . (+ d))

-- Now for the interpreter

-- Get opcode an parameter modes from a value. The head of the list is the mode
-- of the first argument, and so on
getOpcode :: OPCode -> (OPCode, [Mode])
getOpcode fullcode = (opcode, modes)
    where
        opcode = fullcode `mod` 100
        modes = unfoldr (\seed -> Just (seed `mod` 10, seed `div` 10)) (fullcode `div` 100)

-- Given an index and the mode, return the value of a parameter
getParameter :: Int -> Mode -> State ICState OPCode
getParameter idx mode =
    case mode of
        -- Position
        0 -> do
            addr <- getCurrOffset idx
            val <- getMem addr
            -- val <- (getMem <=< (getCurrOffset idx))
            return val
        -- Immediate
        -- I know this is just (getCurrOffset idx), but I think this is clearer
        1 -> do
            val <- getCurrOffset idx
            return val
        -- Unexpected
        _ -> do
            state <- get
            error $ "Unexpected parameter mode: " ++ (show mode) ++ "\n" ++ (show state)

-- Performs one execution step. Return a pair where the first value is False if
-- execution ended and the second one is the (possible) output
step :: State ICState (Maybe Output)
step = do
    currInstr <- getCurrInstr
    let (opcode, modes) = getOpcode currInstr
    case opcode of
        -- HALT
        99 -> return Nothing
        -- ADD
        1 -> do
            let (md1:md2:0:_) = modes
            val1 <- getParameter 1 md1
            val2 <- getParameter 2 md2
            addrres <- getCurrOffset 3
            setMem addrres (val1 + val2)
            incrIP 4
            return Nothing
        -- MUL
        2 -> do
            let (md1:md2:0:_) = modes
            val1 <- getParameter 1 md1
            val2 <- getParameter 2 md2
            addrres <- getCurrOffset 3
            setMem addrres (val1 * val2)
            incrIP 4
            return Nothing
        -- INPUT
        3 -> do
            let (0:_) = modes
            addr <- getCurrOffset 1
            getInput >>= setMem addr
            incrIP 2
            return Nothing
        -- OUTPUT
        4 -> do
            let (md1:_) = modes
            val <- getParameter 1 md1
            incrIP 2
            return $ Just val
        -- JUMP-IF-TRUE
        5 -> do
            let (md1:md2:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            if par1 /= 0 then setIP par2 else incrIP 3
            return Nothing
        -- JUMP-IF-FALSE
        6 -> do
            let (md1:md2:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            if par1 == 0 then setIP par2 else incrIP 3
            return Nothing
        -- LESS-THAN
        7 -> do
            let (md1:md2:0:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            addr <- getCurrOffset 3
            setMem addr $ if par1 < par2 then 1 else 0
            incrIP 4
            return Nothing
        -- EQUAL
        8 -> do
            let (md1:md2:0:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            addr <- getCurrOffset 3
            setMem addr $ if par1 == par2 then 1 else 0
            incrIP 4
            return Nothing
        -- Unexpected
        _ -> do
            state <- get
            error $ "Unexpected opcode: " ++ (show opcode) ++ "\n" ++ (show state)

-- Check whether execution should halt or not
halt :: State ICState Bool
halt = do
    currInstr <- getCurrInstr
    case currInstr `mod` 100 of
        -- HALT
        99 -> return True
        _ -> return False

-- Exec operations: collect output stream
execICfull :: ICProg -> InputStream -> (ICProg, OutputStream)
execICfull prog istream = (icprog, catMaybes ostream)
    where
        initstate = (0, prog, istream)
        (ostream, (_, icprog, _)) = runState (step `untilM'` halt) initstate

execIC :: ICProg -> InputStream -> OutputStream
execIC prog istream = snd $ execICfull prog istream
