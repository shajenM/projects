
module TowerOfHanoi where

import SolutionToAnimation

-- |
-- | Solution for tower of hanoi. 
-- | A series of steps to move disk from one peg to another
-- |
-- | move <disk number> from <fromPeg> using <auxPeg> to <toPeg>
-- | diskNumber {0,1,2 etc }
-- | pegNumbers are {0,1,2}
move 0 a b c = [(0,a,c,b)]
move d a b c = moveToAux ++ [moveDisk]++ moveToPeg
    where moveToAux = move (d-1) a c b
          moveDisk   = ( d,a,c,b )
          moveToPeg  = move (d-1) b a c

-- |
-- | solution for n disks
-- |
solution n = move (n-1) 0 1 2

-- | Test with 3 disks
-- | press space to start animation
-- |
runFor_3_disks = do
    let solutionFor3 = solution 3
        animCommands  = solWithState solutionFor3 3
    runTower animCommands

