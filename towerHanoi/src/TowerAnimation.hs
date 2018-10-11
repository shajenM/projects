

-- Program to animate a square pixel left, right, up and down
-- The Game state stores a list of coordinates for the square and Next direction
--   ( Left or Right or up or down)
--   Vector transformation is used to update co-ordinate for a command

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TowerAnimation (
 DiskNum,
 Command (CUp,CDown,CLeft,CRight,Tick),
 towerAnimation
)  where

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y )
import Data.Sequence (Seq, fromList)
import Brick ( App (..), BrickEvent (..) , AttrMap, 
       EventM, Next, Widget, neverShowCursor, customMain 
     , continue, AttrName , attrMap , str, withAttr
     , hLimit, vLimit, vBox, hBox
     , withBorderStyle
     , on, fg, halt
     )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B

import qualified Graphics.Vty as V
import Control.Monad (void )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent ( MVar, tryPutMVar)
import qualified Data.Map as M


data Game =Game{
    _chan     :: MVar Command,
    _diskset  :: DiskSet, -- map to keep disks with their position
    _work     :: Command
  } 

data Cell      = CellSquare | CellPeg| Empty  -- a cell in a display grid
type Name      = ()
type Coord     = V2 Int    -- coord is 2D-vector
type Disk      = Seq Coord -- disk is a list of coordinates
type DiskSet   = M.Map DiskNum Disk -- all disks are in a map by disk-number
type Steps     = Int
type DiskNum   = Int        -- disk number
type Peg       = Int        -- peg number

-- Tower control commands
data Command   = CLeft  DiskNum 
               | CRight DiskNum 
               | CUp    DiskNum 
               | CDown  DiskNum 
               | Tick
               | Done deriving Show

makeLenses ''Game

height, width :: Int
height = 20 
width  = 29

app :: App Game Command Name
app = App {
       appDraw         = drawUI
     , appChooseCursor = neverShowCursor
     , appHandleEvent  = handleEvent
     , appStartEvent   = return
     , appAttrMap      = const theMap
     }

-- ....................................................
drawUI::Game -> [Widget Name]
drawUI g = [ C.center (drawGrid g) ]

-- ....................................................
handleEvent :: Game -> BrickEvent Name Command -> EventM Name (Next Game )
handleEvent g (AppEvent Tick ) = (replyDone. diskMove ) g
handleEvent g (AppEvent (CLeft  d )) = (continue.setDiskMove (CLeft  d)) g
handleEvent g (AppEvent (CRight d )) = (continue.setDiskMove (CRight d)) g
handleEvent g (AppEvent (CUp    d )) = (continue.setDiskMove (CUp    d)) g
handleEvent g (AppEvent (CDown  d )) = (continue.setDiskMove (CDown  d)) g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

-- ....................................................
diskAttr, pegAttr, emptyAttr :: AttrName
pegAttr    = "pegAttr"
diskAttr   = "diskAttr"
emptyAttr  = "emptyAttr"

-- ....................................................
setDiskMove m (Game ch p w ) = (Game ch p m ) 

-- ....................................................
-- update disk position based on Command.
diskMove :: Game -> Game 
diskMove g@(Game ch diskSet Done ) = g

diskMove g@(Game ch diskSet cmd ) = 
     (Game ch ( newDiskPos cmd ) cmd ) where
     newDiskPos (CUp    diskNum) = M.adjust (trans  vUp)    diskNum diskSet 
     newDiskPos (CDown  diskNum) = M.adjust (trans  vDown)  diskNum diskSet 
     newDiskPos (CLeft  diskNum) = M.adjust (trans  vLeft)  diskNum diskSet 
     newDiskPos (CRight diskNum) = M.adjust (trans  vRight) diskNum diskSet 

-- unit vectors to move up down left or right
vLeft  = V2 (-1) 0
vRight = V2 1    0
vUp    = V2 0    1
vDown  = V2 0    (-1)

-- transform Sequence
trans v p =  fmap (+v) p
-- ....................................................
-- Reply that the command asked is done.( put Done in response channel)
replyDone :: Game -> EventM Name (Next Game )
replyDone g@(Game ch ds Done ) = continue g
replyDone g@(Game ch ds _ ) = do
    liftIO $ tryPutMVar ch Done
    let g2 =(Game ch ds Done ) 
    continue g2

-- ....................................................
-- The grid is the entire square of size 20x25 box
-- The program re-draws this square in every Tick
drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold 
      $ B.borderWithLabel ( str "Tower of Hanoi")
      $ vBox rows
      where 
       -- draw all the cells in the grid.
       -- a cell represent  disk, peg or Empty.
       rows =  [ hBox $ cellsInRow r | r<- [height-1,height-2..0]]
       cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
       drawCoord = drawCell . cellAt 
       cellAt c 
         | isCellPartOfDiskSet c (g^.diskset) = CellSquare
         | isCellPartOfPegSet c = CellPeg
         | otherwise = Empty

       isCellPartOfDiskSet c diskSet = 
           any (==True) $ fmap isPartOfDisk [0,1,2] -- for each disk check 
         where
           isPartOfDisk  i = c `elem` ( diskSet M.! i)
       isCellPartOfPegSet c =  c `elem` towerPegSet

-- draw a cell
drawCell :: Cell -> Widget Name 
drawCell CellSquare = withAttr diskAttr cw
drawCell CellPeg    = withAttr pegAttr cw
drawCell Empty      = withAttr emptyAttr cw
-- pixel is 2 spaces
cw :: Widget Name
cw = str "  "

-- ....................................................
-- coordinate system  (x,y)
-- 00,19            19,19    
--
--          19,19
--
-- 00,00            19,00

-- build tower disk set with smallest disk on top
disk0=[                     V2 04 11 , V2 05 11, V2 06 11                     ]
disk1=[           V2 03 10, V2 04 10 , V2 05 10, V2 06 10, V2 07 10           ] 
disk2=[ V2 02 09, V2 03 09, V2 04 09 , V2 05 09, V2 06 09, V2 07 09, V2 08 09 ] 

towerDiskSet = M.fromList [(0,fromList disk0), (1,fromList disk1), (2,fromList disk2)]

peg0    = fmap (\y-> V2 05 y ) [ 12,11,10,09 ] -- peg at col 05
peg1    = fmap (\y-> V2 14 y ) [ 12,11,10,09 ] -- peg at col 14
peg2    = fmap (\y-> V2 23 y ) [ 12,11,10,09 ] -- peg at col 23
pegBase = fmap (\x-> V2 x 08 ) [ 1..27 ]       -- peg base at row 08

towerPegSet  = peg0 ++ peg1 ++ peg2 ++ pegBase

-- ....................................................
-- initGame :: MVar Command -> Game
initGame ch =  Game ch towerDiskSet Done

-- ....................................................
-- this is the color of different items on the screen
-- first one is fore-ground second is back-ground
-- emptyAttr is a default color which is backgound of the original screen

darkBlack = V.rgbColor 0 0 0
grey = V.rgbColor 80 50 50

theMap :: AttrMap
theMap = attrMap V.defAttr [ 
    (diskAttr,   V.yellow  `on` V.yellow)
   ,(pegAttr,    darkBlack  `on`  darkBlack )
   ,(emptyAttr,  grey `on` grey )
  -- ,(emptyAttr,  V.black `on` V.black )
  ]

-- ....................................................
towerAnimation req resp=do
  void $ customMain (V.mkVty V.defaultConfig)
        (Just req) app ( initGame resp)
