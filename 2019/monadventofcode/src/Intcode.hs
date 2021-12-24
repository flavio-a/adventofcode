module Intcode (
    parse,
    showICProg,
    execIC,
    execICfull,
    (//)
    )
where

import Data.List (intercalate)
import Data.List.Split (splitOn, splitOneOf)
import Data.Array.Unboxed
import Control.Monad.State.Lazy
import Control.Monad.Extra

type OPCode = Int
type IP = Int
type Addr = Int
type ICProg = UArray Addr OPCode

parse :: String -> ICProg
parse str = listArray (0, length list - 1) list
    where
        list = map read $ splitOn "," str

showICProg :: ICProg -> String
showICProg = intercalate "," . fmap show . elems

-- I would like a monad in which execute instructions
-- The monad is State, where my state is a pair (IP, memory)
type ICState = (IP, ICProg)

getIP :: State ICState IP
getIP = get >>= (return . fst)

setIP :: IP -> State ICState ()
setIP newip = modify (\(_, mem) -> (newip, mem))

incrIP :: Int -> State ICState ()
incrIP d = modify (\(oldip, mem) -> (oldip + d, mem))

getMem :: Addr -> State ICState OPCode
getMem addr = get >>= (return . (! addr) . snd)

setMem :: Addr -> OPCode -> State ICState ()
setMem addr newop = modify (\(ip, oldmem) -> (ip, oldmem // [(addr, newop)]))
-- setMem addr newop = do
--     (pc, oldmem) <- get
--     put (pc, oldmem // [(addr, newop)])

-- Get the current instruction
getCurrInstr :: State ICState OPCode
getCurrInstr = getIP >>= getMem

-- Get values in memory at the current position plus an offset
getCurrOffset :: Int -> State ICState OPCode
getCurrOffset d = getIP >>= (getMem . (+ d))

-- Now for the interpreter

-- Performs one execution step, and return False if execution ended
step :: State ICState Bool
step = do
    currInstr <- getCurrInstr
    case currInstr of
        -- HALT
        99 -> return False
        -- ADD
        1 -> do
            addr1 <- getCurrOffset 1
            addr2 <- getCurrOffset 2
            val1 <- getMem addr1
            val2 <- getMem addr2
            addrres <- getCurrOffset 3
            setMem addrres (val1 + val2)
            incrIP 4
            return True
        -- MUL
        2 -> do
            addr1 <- getCurrOffset 1
            addr2 <- getCurrOffset 2
            val1 <- getMem addr1
            val2 <- getMem addr2
            addrres <- getCurrOffset 3
            setMem addrres (val1 * val2)
            incrIP 4
            return True
        -- Unexpected
        _ -> error $ "Unexpected opcode: " ++ (show currInstr)

execICfull :: ICProg -> ICProg
execICfull prog = snd $ execState (whileM step) (0, prog)

execIC :: ICProg -> Int
execIC = (! 0) . execICfull
