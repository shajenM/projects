
{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SolutionToAnimation (
    runTower,
    solWithState
) where 

import Brick.BChan (newBChan, writeBChan)

import Control.Monad (forever )
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)

import TowerAnimation

sendCommand cmdReq cmdResp c = do
    writeBChan cmdReq c 
    takeMVar cmdResp
    return ()
-- |
-- | send a command to move disk to animation system
moveDisk c req resp = (sendCommand req resp) c 

moveDiskNtimes c r p 1 =  moveDisk c r p
moveDiskNtimes c r p n =  do 
    moveDisk c r p
    moveDiskNtimes c r p (n-1)

moveDiskN c n = \r p -> moveDiskNtimes  c r p n
-- |
-- | send commands to move disk up, down, left or right
moveDiskUp    d n = moveDiskN (CUp d) n
moveDiskDown  d n = moveDiskN (CDown d) n
moveDiskLeft  d n = moveDiskN (CLeft d) n
moveDiskRight d n = moveDiskN (CRight d) n
-- |
-- | send a series of disk move commands to animation system
runAnimCommands [] req resp = return ()
runAnimCommands (c:cs) req resp = do
    let (d,f,t,pegState ) = c
    pegToPeg_anim d f t pegState req resp
    runAnimCommands cs req resp
-- ....................................................
-- |
-- | solWithState adds peg state information to the solution
-- | for example , in state (0, 1, 2 (3,0,0) ) where
-- |     (0,1,2, pegState) means instruction to move disk 0 from peg 1 to peg 2
-- |      pegState (3,0,0) means peg1 has 3 disks, 
-- |                             peg2 has 0, and peg 3 and 0 diks
-- |
-- | next state becomes (1,1,2 (2,1,0) ) where
-- |     (1,1,2, pegState) means instruction to move disk 1 from peg 1 to peg 2
-- |     peg state (2,1,0) means peg 1 has 2 disks, 
-- |                             peg2 has 1 disk, peg3 has 1 disk
-- | 
solWithState sols n = scanl stateFn (0,0,0,(n,0,0)) sols where
    stateFn (d0,f0,t0,(p1,p2,p3)) (d,f,t,x) = (d,f,t,pegState) where
        pegState = (p1+a1, p2+a2, p3+a3)  where
            (a1,a2,a3) = adder f0 t0 where
                   adder 0 1 = (-1, 1, 0)
                   adder 0 2 = (-1, 0, 1)
                   adder 1 2 = ( 0,-1, 1)
                   adder 1 0 = ( 1,-1, 0)
                   adder 2 0 = ( 1, 0,-1)
                   adder 2 1 = ( 0, 1,-1)
                   adder 0 0 = ( 0, 0, 0)

pegToPeg_anim d f t pegState
    | t > f = animDiskToRight d f t pegState
    | t < f = animDiskToLeft  d f t pegState where

    animDiskToRight d f t pegState = \req resp -> do
        moveDiskUp    d up req resp 
        moveDiskRight d right req resp 
        moveDiskDown  d down req resp 

    animDiskToLeft d f t pegState = \req resp -> do
        moveDiskUp    d up req resp 
        moveDiskLeft  d left req resp 
        moveDiskDown  d down req resp 
    up    = 5-(get f pegState)
    down  = 4-(get t pegState)
    right = (t-f)*9
    left  = (f-t)*9

get 0 (p1,p2,p3) = p1
get 1 (p1,p2,p3) = p2
get 2 (p1,p2,p3) = p3
-- |
-- | Run Aninamtion system with anim commands
-- |
runTower :: [(TowerAnimation.DiskNum, Integer, Integer,
               (Integer, Integer, Integer))]-> IO ()
runTower animCommands = do
    cmdReq  <- newBChan 20
    cmdResp <- newEmptyMVar
    -- Tick timing loop
    forkIO $ forever $ do
        writeBChan cmdReq Tick
        threadDelay (1000*150)
    -- send anim commands loop
    forkIO $ do 
        getChar
        runAnimCommands (tail animCommands) cmdReq cmdResp
    towerAnimation cmdReq cmdResp


