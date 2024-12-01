module Intcode (
    ICProg,
    ICState,
    Input,
    InputStream,
    OutputStream,
    parse,
    showICProg,
    (//),
    (!),
    execIC,
    execICfull,
    initICvm,
    execICinput,
    )
where

import Data.List (intercalate, unfoldr)
import Data.List.Split (splitOn, splitOneOf)
import Data.Array.Unboxed
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (isSpace)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State.Lazy
import Control.Monad.Loops (untilM')

type OPCode = Int
type IP = Int
type Addr = Int
type ICProg = UArray Addr OPCode
type Mode = Int

type Input = Int
type InputStream = [Input]
type Output = Int
type OutputStream = [Output]

parse :: String -> ICProg
parse str = listArray (0, length list - 1) list
    where
        list = map read $ splitOn "," $ filter (not . isSpace) str

showICProg :: ICProg -> String
showICProg = intercalate "," . fmap show . elems

-- I would like a monad in which execute instructions
-- The monad is State, where my state is a struct
data ICState = ICState { ip :: IP
                       , prog :: ICProg
                       , extramem :: IntMap OPCode
                       , istream :: InputStream
                       , rbase :: Addr
                       } deriving Show
-- Base ICState
baseICState :: ICState
baseICState = ICState { ip = 0, extramem = IntMap.empty, rbase = 0 }

getIP :: State ICState IP
getIP = gets ip

setIP :: IP -> State ICState ()
setIP newip = modify (\state -> state { ip = newip })

incrIP :: Int -> State ICState ()
incrIP d = modify (\state -> state { ip = ip state + d })

getInput :: State ICState Input
getInput = do
    state <- get
    put $ state { istream = tail $ istream state }
    return $ head $ istream state

getRbase :: State ICState Addr
getRbase = gets rbase

incrRbase :: Addr -> State ICState ()
incrRbase d = modify (\state -> state { rbase = rbase state + d })

-- Working with memory: should decide whether to use the program array or the
-- hashmap
isInArray :: Addr -> ICProg -> Bool
isInArray addr prog = inRange (bounds prog) addr

getMem :: Addr -> State ICState OPCode
getMem addr = do
    state <- get
    if inRange (bounds $ prog state) addr then
        return (prog state ! addr)
    else
        return (fromMaybe 0 $ IntMap.lookup addr $ extramem state)

setMem :: Addr -> OPCode -> State ICState ()
-- setMem addr newop = modify (\state -> state { prog = (prog state) // [(addr, newop)] })
setMem addr newop = modify updateMem
    where
        updateMem :: ICState -> ICState
        updateMem state = if inRange (bounds $ prog state) addr then
                              state { prog = (prog state) // [(addr, newop)] }
                          else
                              state { extramem = IntMap.insert addr newop (extramem state) }

-- Get the current instruction
getCurrInstr :: State ICState OPCode
getCurrInstr = getIP >>= getMem

-- Get values in memory at the current position plus an offset
getCurrOffset :: Int -> State ICState OPCode
getCurrOffset d = getIP >>= (getMem . (+ d))

------------------------------- Interpreter -----------------------------------

-- Get opcode an parameter modes from a value. The head of the list is the mode
-- of the first argument, and so on
getOpcode :: OPCode -> (OPCode, [Mode])
getOpcode fullcode = (opcode, modes)
    where
        opcode = fullcode `mod` 100
        modes = unfoldr (\seed -> Just (seed `mod` 10, seed `div` 10)) (fullcode `div` 100)

-- Given an index and the mode, return the address of a parameter
getAddress :: Int -> Mode -> State ICState Addr
getAddress idx mode =
    case mode of
        -- Position
        0 -> getCurrOffset idx
        -- Relative
        2 -> do
            rbase <- getRbase
            delta <- getCurrOffset idx
            return $ rbase + delta
        -- Unexpected
        1 -> do
            state <- get
            error $ "Asked address for parameter mode 1 (immediate)\n" ++ show state
        _ -> do
            state <- get
            error $ "Unexpected parameter mode for address: " ++ show mode ++ "\n" ++ show state

-- Given an index and the mode, return the value of a parameter
getParameter :: Int -> Mode -> State ICState OPCode
getParameter idx mode =
    case mode of
        -- Position
        0 -> getAddress idx 0 >>= getMem
        -- Immediate
        1 -> getCurrOffset idx
        -- Relative
        2 -> getAddress idx 2 >>= getMem
        -- Unexpected
        _ -> do
            state <- get
            error $ "Unexpected parameter mode for value: " ++ (show mode) ++ "\n" ++ (show state)

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
            let (md1:md2:md3:_) = modes
            val1 <- getParameter 1 md1
            val2 <- getParameter 2 md2
            addrres <- getAddress 3 md3
            setMem addrres (val1 + val2)
            incrIP 4
            return Nothing
        -- MUL
        2 -> do
            let (md1:md2:md3:_) = modes
            val1 <- getParameter 1 md1
            val2 <- getParameter 2 md2
            addrres <- getAddress 3 md3
            setMem addrres (val1 * val2)
            incrIP 4
            return Nothing
        -- INPUT
        3 -> do
            let (md:_) = modes
            addr <- getAddress 1 md
            setMem addr =<< getInput
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
            let (md1:md2:md3:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            addr <- getAddress 3 md3
            setMem addr $ if par1 < par2 then 1 else 0
            incrIP 4
            return Nothing
        -- EQUAL
        8 -> do
            let (md1:md2:md3:_) = modes
            par1 <- getParameter 1 md1
            par2 <- getParameter 2 md2
            addr <- getAddress 3 md3
            setMem addr $ if par1 == par2 then 1 else 0
            incrIP 4
            return Nothing
        -- CHANGE-RBASE
        9 -> do
            let (md1:_) = modes
            incrRbase =<< getParameter 1 md1
            incrIP 2
            return Nothing
        -- Unexpected
        _ -> do
            state <- get
            error $ "Unexpected opcode: " ++ (show opcode) ++ "\n" ++ (show state)

-- Check whether execution should halt or not
halt :: State ICState Bool
halt = do
    currInstr <- getCurrInstr
    let (opcode, _) = getOpcode currInstr
    case opcode of
        -- HALT
        99 -> return True
        _ -> return False

-- Initialize an Intcode VM
initICvm :: ICProg -> InputStream -> ICState
initICvm prog istream = baseICState { prog = prog, istream = istream }

-- Exec operations: collect output stream
execICfull :: ICProg -> InputStream -> (ICProg, OutputStream)
execICfull myprog istream = (prog finstate, catMaybes ostream)
    where
        (ostream, finstate) = runState (step `untilM'` halt) $ initICvm myprog istream

-- Can't use the implementation `execIC = snd . execICfull` because runState
-- isn't lazy enough to support connection of input and output streams
execIC :: ICProg -> InputStream -> OutputStream
execIC prog istream = catMaybes $ evalState (step `untilM'` halt) $ initICvm prog istream

---------------------------- Interactive input --------------------------------

-- Runs an IC vm, blocking when it needs an input but the input stream is empty
execICinput :: ICState -> InputStream -> (OutputStream, ICState)
execICinput initstate newistream = (catMaybes ostream, finstate)
    where
        initstate' = initstate { istream = newistream }
        (ostream, finstate) = runState (step `untilM'` wait) initstate'

        wait :: State ICState Bool
        wait = do
            currInstr <- getCurrInstr
            let (opcode, _) = getOpcode currInstr
            case opcode of
                99 -> return True
                3  -> gets (null . istream)
                _  -> return False
