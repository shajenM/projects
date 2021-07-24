-- # DinoBoyGame
-- # Author : Shajen Muttath
-- # 
module DinoBoyGame
    (rungame) 
where
import Hudak.UserEvent as UE
import Hudak

-- game states
data GameState =  
    Collision
    | Pause
    | Running
    | Score
    deriving (Eq,Show)

state = constB Running `switch` (onBoxCollision .|. onCoinCollision .|. onResume) where
    onBoxCollision  = whenBoxCollide ->> constB Collision 
    onCoinCollision = whenCoinCollide ->> constB Score
    onResume        = whenResume ->> constB Running

lastCollision :: Behavior Time
lastCollision = 0 `switch` ((whenState Collision) `snapshot_` time ) =>> constB where

whenState st = whenB (lift1 isState state) where
    isState s = s==st

whenResume = whenB ((time - lastCollision) >* 5) 
-- ------------------------------------------------------------------
-- find last active time
-- it takes a snapshot of time when event happened.
--  idle time is obtained by taking difference with current time and lastActimeTime
lastActive = 0 `switch` (onUp .|. onSpaceKey) where
    onUp       = ((whenArrow UE.Up) `snapshot_` time) =>> constB
    onSpaceKey = ((keyPressed ' ') `snapshot_` time) =>> constB

whenIdle = whenB ((time - lastActive) >* 10) 
        -- raise event when (idle time > 10 seconds)
-- -----------------------------------------------------------
-- game images
-- background
--bgSkin = constB 0
bgImage = backgroundB (constB resBgImage) bgMove
-- 
bgSpeed = lift1 round  ((elapsed 10)*150)

bgMove = bgSpeed `switch` (onCollide .|. onRunning) where 
    onCollide = (whenState Collision)  ->> constB 0
    onRunning = (whenState Running) ->> bgSpeed

bgGroundY = (-0.20)::Float  -- y position of ground

-- boy sound
ouchSound = playB (constB resAudioOuch)
boySound1 = noSound `modulate` (whenState Collision ->> ouchSound)
-- boySound1 = noSound `modulate` (whenB (boyYPos >* bgGroundYB ) ->> ouchSound)
boySound2 = noSound `modulate` (whenState Collision ->> wetSound)

boySound = boySound1 -- `over` boySound2

-- boy sprite
-- change skin at every given seconds
changeSkinRate t = constB 0 `switch` ((timer t ) `withElem_`  cycle items ) where
    items = map constB [1,2,3,4,5,0]

boySkin = 0 `switch` (onGround .|. onAir) where
    onAir    = (keyPressed ' ') ->> changeSkinRate 0.2
    onGround = whenB (boyYPos >* bgGroundYB ) ->> constB 0
    nextSkin n = constB ((n+1) `mod` 6)

faceLeft = constB True
faceRight = constB False

idleFace = faceLeft `switch` (timer 3 `withElem_`  cycle [faceLeft, faceRight])
runningFace = faceRight

boyFaceDir = faceRight `switch` (onIdle .|.onRunning) where
    onIdle    = whenIdle          ->> idleFace
    onRunning = whenState Running ->> runningFace

boyImage = spriteB (constB resBoyImage) boySkin boyFaceDir
-- y values during a jump
-- when space pressed, switch y to jump behavior
g = 9
vel0 = constB (-4)
bgGroundYB = constB bgGroundY
boyVel =  0.0 `switch` (onHit .|. onUp .|. onCollide)  where 
    onHit     = whenB (boyYPos >* bgGroundYB ) ->> constB 0.0 
        -- stop moving (velocity 0 )when y > 1.
        -- note that, when I used 'while' instead of whenB,  pressing Space not worked!
    onUp      = keyPressed ' '       ->> vel0 + integral g
    onCollide = whenState Collision  ->> constB 0.0

boyYPos :: Behavior Float
boyYPos = bgGroundYB + integral boyVel 

-- check if any object collide with boy
-- here we could use lift4, but we can achive same using lift0 and operator ($*)
whenBoyCollideWith objX1 objY1 objX2 objY2 = 
    whenB (lift0 isCollide $* objX1 $* objY1 $* objX2 $* objY2 $* boyXPos $* boyYPos)
isCollide ox1 oy1 ox2 oy2 boyx1 boyy1  = 
    isRectIntersect (ox1,oy1) (ox2,oy2) (boyx1,boyy1) (boyx1+0.93,boyy1+0.93)

-- moving targets
whenBoxCollide = (whenBoyCollideWith box1X box1Y box1_X2 box1_Y2)  .|. (whenBoyCollideWith box2X box2Y box2_X2 box2_Y2)

boxVel v = moveSpeed v `switch` (onCollide .|. onRunning) where -- velocity is 2 inches per second
    onCollide = whenState Collision  =>> const 0.0 
    onRunning = whenState Running    ->> moveSpeed v

box1X = boxVel 1
box1Y = constB (-0.25)
box1_X2 = box1X + 0.2
box1_Y2 = constB (-0.25 + 1.0)
box1 = paintedPicture brown  (translate (box1X, box1Y) (rectFill 0.2 1.0) ) 

box2Y = constB (0.25)
box2X = boxVel 5
box2_X2 = box2X + 0.2
box2_Y2 = constB (-0.25 + 0.5)
box2_Color = aquamarine
box2 = paintedPicture box2_Color  (translate (box2X, box2Y) (rectFill 0.2 0.5) ) 

coin1_X1 = boxVel 3
coin1_Y1 = constB (-0.25)
coin1_X2 = coin1_X1 + 0.2
coin1_Y2 = constB (-0.25 + 0.2)

whenCoin1Collide = whenBoyCollideWith coin1_X1 coin1_Y1 coin1_X2 coin1_Y2
goldDark = constB GoldD
goldLight = constB GoldL
twingling = goldLight `switch` ( timer 1.5 `withElem_` cycle[gold, goldLight])
coinRotation = cos time * 0.08 + 0.1

coin1 = paintedPicture twingling  (translate (coin1_X1, coin1_Y1) (ell coinRotation 0.2 ) ) 

coin2_X1 = boxVel 2
coin2_Y1 = constB (0.5)
coin2_X2 = coin2_X1 + 0.1
coin2_Y2 = constB (0.5 + 0.05)

whenCoin2Collide = whenBoyCollideWith coin2_X1 coin2_Y1 coin2_X2 coin2_Y2
coin2Rotation = cos time * 0.04 + 0.1
coin2 = paintedPicture twingling  (translate (coin2_X1, coin2_Y1) (ell coin2Rotation 0.05 ) ) 

wetSound = playB (constB resAudioWetSplash)
coinSound = noSound `modulate` (whenCoinCollide ->> wetSound)

-- common coins
whenCoinCollide = whenCoin1Collide .|. whenCoin2Collide
coins = coin1 `over` coin2
-- game messages
collisionMsg = emptyMsg  `switch` (onUp .|. onResume) where
    onUp     = whenBoxCollide ->> txtCollisionMsg
    onResume = whenState Running ->>  emptyMsg

txtCollisionMsg = txtMsgAt (-1.5) 0 font50B red (txtB "You Died!!")
emptyMsg = constB EmptyPic

boy22 = rectFill 0.5 0.5

boyXPos = constB (-1.00)
--boyImageScaled = scale 0.5 boyImage
boySprite = paintedPicture blue (translate (boyXPos,  boyYPos) boyImage )
backGrd   = paintedPicture yellow (translate (-5.00, -2.50) bgImage)

 
-- score
scoreBoxColor = constB TransBlack
scoreBox = paintedPicture scoreBoxColor  (translate  (-4.3, -2.43) (rectFill 1.0 0.3))
scoreMsg = txtMsgAt (-4) (-2.5) font30B magenta (intMsgB scoreCount)
scoreBorad = scoreBox `over` scoreMsg
-- score = txtMsgAt (-4) (-2.5) black (mmPos)

scoreCount = 0.00  `stepAccum` (onCoin .|. onResume ) where
    onCoin   = whenCoinCollide ->> (+1.0)     -- increase credit when ht on coin
    onResume = whenResume  ->> const 0.0  -- credits lose when hit on box, but show when resume

-- debug
debugMsg = lift2 f box1X boyXPos where
    f x y = strToTxt ("boxX "++(floatToStr x)++", boyX"++(floatToStr (y+1.1)))

-- game application
boxes    = box1 `over` box2 `over` coins
sounds   = boySound `over` coinSound
messages = collisionMsg `over` scoreBorad

game  = backGrd `over` boySprite `over` boxes `over` sounds `over` messages 

-- run game
rungame = runReact  "Reactive Game" game resources

