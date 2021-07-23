module Hudak where
import Util
import Fal
import Shape
import Region
import Picture
import Reactimate
import SDLDraw
import qualified Data.Text as T

-- test reactive Behavior
runReact title beh res = reactimate title beh res picToGraphic


floatMsgB = lift1 floatToTxt

roundB :: Behavior Float -> Behavior Int
roundB    = lift1 round
intMsgB   = lift1 (strToTxt.intToStr.round)
txtB str = constB (T.pack str)
emptyStrB = (txtB "")

-- nosound
noSound = lift1 SoundP (constB Empty)

-- usefull items on screen
txtMsg f s = txtMsgAt (-3.00) (-2.50) f magenta s
txtMsgAt x y f c s = paintedPicture c (translate (x, y) (text f s))

-- show the mouse position
mmPos = txtB "0 0" `switch` (mm =>> f ) where
    f (x,y) = txtB ((floatToStr x)++", "++(floatToStr y))

withItems (x:xs) t = constB x `switch` ((timer t ) `withElem_`  cycle items ) where
    items = map constB (xs++[x])

-- move at v inches per second ( ie velocity) to a distance of 10 inches (current screen width)
moveSpeed v = 5-(elapsed (10/v))*v
moveSpeedD v d = d/2-(elapsed (d/v))*v

-- integral
integral :: Behavior Float -> Behavior Float
integral (Behavior fb)
  = Behavior (\uts@(us,t:ts) -> 0 : loop t 0 ts (fb uts))
      where loop t0 acc (t1:ts) (a:as) 
                 = let acc' = acc + (t1-t0)*a
                   in acc' : loop t1 acc' ts as

-- ------------------------------
-- test collision
-- does 2 rectangles by points intersect?
isRectIntersect (ax1,ay1) (ax2,ay2) (bx1,by1) (bx2,by2) =
     let c1 = isRangeIntersect (ax1,ax2) (bx1,bx2)
         c2 = isRangeIntersect (ay1,ay2) (by1,by2)
     in  c1 && c2 

-- Check if range a-b is inside c-d or otherway
-- range 2-3 and range 1-5 intersect
-- range 2-3 and range 4-5 do not intersect
isRangeIntersect (a,b) (c,d) =
     let c1 = inBetween a c d 
         c2 = inBetween b c d
         c3 = inBetween c a b
         c4 = inBetween d a b
     in  c1 || c2 || c3 || c4

-- check if a number is between 2 numbers
-- 2 1 3,  2 is inBetween 1 and 3
-- 2 3 1,  2 is inBetween 3 and 1 
inBetween n a b = let d = abs(a-b) 
                      d1 = abs(a-n)
                      d2 = abs(b-n)
                  in  d1 <= d && d2 <= d
-- load the resources
resFont50 = Font 70
resFont30 = Font 30
resFont20 = Font 20

font50B = constB resFont50
font30B = constB resFont30
font20B = constB resFont20

bg_image_name = strToTxt "open_field.jpg"
-- boy_image_name = strToTxt "boy1.png"
boy_image_name = strToTxt "boy_anim.png"

resBoyImage = BoyImage boy_image_name
resBgImage  = BackGround bg_image_name

audio_ouch_name = strToTxt "ouch.mp3"
resAudioOuch = Audio audio_ouch_name

audio_wetsplash_name = strToTxt  "doubleTing.mp3"
resAudioWetSplash = Audio audio_wetsplash_name

resources e = resourceToSDLResource resFont50 e 
    >>= resourceToSDLResource resFont20
    >>= resourceToSDLResource resFont30
    >>= resourceToSDLResource resBoyImage
    >>= resourceToSDLResource resBgImage
    >>= resourceToSDLResource resAudioOuch
    >>= resourceToSDLResource resAudioWetSplash







