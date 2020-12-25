module AoCAssemblyStateful (
    module AoCAssembly,
    module Control.Monad.State.Lazy,
    ASM,
    execInstrASM,
    execProgASM,
    ) where

import Control.Monad.State.Lazy
import Control.Monad
import AoCAssembly

-- Abstract State Machine for the AoC assembly. Its state is MachineState, it's
-- output is MachineState too
type ASM = State MachineState

-- Abstract State Machine execution of an instruction
-- execInstrASM :: Instruction -> MachineState -> ASM ()
-- execInstrASM instr (acc, idx) = do
execInstrASM :: Instruction -> ASM ()
execInstrASM instr = do
    (acc, idx) <- get
    let (acc', delta) = execInstr instr acc
    put (acc', idx + delta)

execProgASM :: Program -> ASM ()
execProgASM prog = do
    (acc, idx) <- get
    let (acc', delta) = execInstr (prog ! idx) acc
    let idx' = idx + delta
    put (acc', idx')
    if not $ inRange (bounds prog) idx' then
        return ()
    else
        execProgASM prog
