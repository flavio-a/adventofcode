import ReadFile
import Intcode (ICState, Input, InputStream, execICinput, initICvm, parse)
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Array

type Addr = Int

makePackets :: [a] -> [(a, [a])]
makePackets (addr:x:y:rest) = (addr, [x, y]):makePackets rest
makePackets [] = []

type Network = (Array Int ICState, [(Addr, InputStream)])

pingAll = [(i, [-1]) | i <- [0..49]]

networkStep :: State Network ()
networkStep = do
    -- If there's no packet in the network, the rte sends a -1 to each node
    packets <- fmap (\l -> if null l then pingAll else l) $ gets snd
    let (addr, payload) = head packets
    vms <- gets fst
    let (output, newvm) = execICinput (vms ! addr) payload
    let newvms = vms // [(addr, newvm)]
    let newpackets = tail packets ++ makePackets output
    put (newvms, newpackets)

packetTo :: Addr -> State Network Bool
packetTo addr = gets (not . null . filter ((== addr) . fst) . snd)

getPacketTo :: Addr -> [(Addr, InputStream)] -> InputStream
getPacketTo addr = snd . fromJust . find ((== addr) . fst)


data NetworkNAT = NetworkNAT { nodes :: Array Int ICState
                             , flow :: [(Addr, InputStream)]
                             , nat :: InputStream
                             , pinged :: Bool
                             , natSent :: InputStream
                             , finished :: Bool
                             } deriving Show

networkStepWithNAT :: State NetworkNAT ()
networkStepWithNAT = do
    packets <- gets flow
    -- If there's no packet in the network, the rte pings all nodes
    if null packets then do
        isPinged <- gets pinged
        if isPinged then
            -- The network was already pinged: it's idle
            networkNATIdle
        else
            modify (\state -> state { flow = pingAll, pinged = True })
    else let (addr, payload) = head packets in
        if addr == 255 then
            modify (\state -> state { nat = payload, flow = tail packets })
        else do
            vms <- gets nodes
            let (output, newvm) = execICinput (vms ! addr) payload
            let newvms = vms // [(addr, newvm)]
            let newpackets = tail packets ++ makePackets output
            isPinged <- gets pinged
            modify (\state -> state { nodes = newvms, flow = newpackets, pinged = if null output then isPinged else False })

networkNATIdle :: State NetworkNAT ()
networkNATIdle = do
    natContent <- gets nat
    natSentLast <- gets natSent
    when (natContent == natSentLast) $ modify (\state -> state { finished = True })
    modify (\state -> state { flow = [(0, natContent)], pinged = False, natSent = natContent })

networkNATFinished :: State NetworkNAT Bool
networkNATFinished = gets finished


main = do
    content <- ReadFile.readFileArgDefault "inputs/day23"
    let prog = Intcode.parse content
    -- Part 1
    let initvms = listArray (0, 49) [ initICvm prog [] | i <- [0..49] ]
    let lastPackets = snd $ execState (networkStep `untilM_` packetTo 255) $ (initvms, [ (i, [i]) | i <- [0..49] ])
    putStrLn $ show $ (!! 1) $ getPacketTo 255 lastPackets
    -- Part 2
    let initstate = NetworkNAT { nodes = initvms, flow = [ (i, [i]) | i <- [0..49] ], nat = [], pinged = False, natSent = [], finished = False }
    let finishedState = execState (networkStepWithNAT `untilM_` networkNATFinished) initstate
    putStrLn $ show $ (!! 1) $ natSent finishedState
