{-# LANGUAGE MagicHash #-}

module Hudak.Fal where


import qualified Data.Text as T

import Hudak.Picture as Picture
import Hudak.Util
import Hudak.UserEvent as UserEvent
import Data.IORef
import System.IO.Unsafe
import GHC.Prim -- For reallyUnsafePtrEquality
import Hudak.SDLDraw 

--type Time = Float
type UserAction = UserEvent

-- Behavior is a function that takes (stream,stream) as input and produce a 
-- single stream
-- So Behavior you can think that, they are different kinds of functions that
-- convert (stream,stream) into (stream)
-- Yes, a wrapper for different kinds of functions!, which is functional programming!
-- The resulting stream is a list [b1, b2, b3, b4, .....]
--   where b1,b2, b3 are the Pictures that we can draw on screen

newtype Behavior a
    = Behavior (([Maybe UserAction], [Time]) -> [a])

-- Event is a function that can be applied to a stream of OS events (key press or mouse move)
-- (stream,stream) and produce a single stream of values.
-- The resulting stream look like [Just Button, Nothing, Nothing, Just Key]
-- Just means, event happend. Nothing means event did not happen
-- Notice that both presence and absence are recorded
newtype Event a 
    = Event (([Maybe UserAction], [Time]) -> [Maybe a])

--Behaviors----------------------
-- Behavior is a fuction that take (stream,stream) as input
-- Notice that Time behavior has no interest in user action stream uas.
-- This make sense because, ellipse circle etc need only the time part
-- to create their coordinates
time :: Behavior Time
time = Behavior (\(_,ts) -> ts)

constB :: a -> Behavior a
constB x = Behavior (\_ -> repeat x)

-- Graphic behaviors
-- COLOR
red, blue :: Behavior Picture.Color
red = constB Picture.Red
blue = constB Picture.Blue
yellow = constB Picture.Yellow
white = constB Picture.White
green = constB Picture.Green
peach = constB Picture.Peach
brown = constB Picture.Brown
magenta = constB Picture.Magenta
black = constB Picture.Black
aquamarine = constB Picture.Aquamarine
persianIndigo = constB PersianIndigo
gold = constB Gold
addColorB = lift2 addColor

-- SHAPES
shape :: Behavior Shape -> Behavior Region
shape = lift1 Region

ell :: Behavior Float -> Behavior Float -> Behavior Region
ell x y = shape (lift2 Ellipse x y)

rect w h = shape (lift2 Rectangle w h)
rectFill w h = shape (lift2 RectangleFill w h)

-- TEXT
text font str = shape (lift2 Text font str)

-- IMAGE
spriteB res skin flip = shape (lift3 SpriteImage res skin flip)
backgroundB res skin = shape (lift2 BackgroundImage res skin)

-- SOUND
playB res = lift1 SoundP (shape (lift1 Sound res))
--
-- -----------------------------------------------------------------
translate :: (Behavior Float, Behavior Float) -> Behavior Region -> Behavior Region
translate (Behavior fx, Behavior fy) (Behavior fp) =
    Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
        where aux x y p = Translate (x,y) p

scale s r = lift2 Scale (pairB s s) r

paintedPicture :: Behavior Picture.Color -> Behavior Region -> Behavior Picture
paintedPicture = lift2 Picture

over = (lift2 Over )

-- TIMER

-- elapsed 10 will give, running float numbers till 10 seconds. Values like
--  [0.000, 0.001,0.002,..,0.999,1.000, 1.001, 1.002,...,9.000,9.01,9.002, 9.999]
-- each number is seconds and milliseocnds
-- After 10 seconds, it reset to 0.000 and continues same sequence
-- It make sure that 9.999 reaches at 10th second
elapsed :: Behavior Time -> Behavior Time
elapsed n = time - t2  where 
        t2 = 0.0 `switch` ( ( evt `snapshot_` time) =>> constB )
        evt = whenB (time-t2 >* n)

-- timer 5 will give a event at evry 5 second
timer:: Behavior Time -> Event ()
timer n = while ( elapsed n >* n)
-- timer issues
-- when I used timer instead of elapsed, it was working but
--      rate of events raising varies when I move nouse pointer!!
-- MATH
-- less than
a LT b = lift2 (<)

instance Num a => Num (Behavior a) where
    (+) = lift2 (+)
    (*) = lift2 (*)
    negate = lift1 negate
    abs = lift1 abs
    signum = lift1 signum
    fromInteger = lift0.fromInteger
    
instance Fractional a => Fractional (Behavior a) where
    (/) = lift2 (/)
    fromRational = lift0.fromRational

instance Floating a => Floating (Behavior a) where
    cos = lift1 cos
    sin = lift1 sin

-- oprerator precedence
infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
--infixr 3 &&*
--infix 2 ||*
-- Lifting operators

-- Notice that basic behavior is to zipWith.
-- This is different from applicative for list
-- zipWith f [a] [b] will apply f(a,b) and create list [f(a,b), ...]
-- (zipWith $) has signature:
-- zipWith ($) :: [a -> b] -> [a] -> [b]
--   it expects a function f(a) in first list and 'a' in second list
--   and does f $ a, which is f(a) and creates list [b]
($*) :: Behavior (a->b) -> Behavior a -> Behavior b
Behavior ff $* Behavior fb =
    Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

lift0 :: a -> Behavior a
lift0 = constB

lift1 :: (a->b) -> Behavior a -> Behavior b
lift1 f b1 = lift0 f $* b1

lift2 :: (a-> b-> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2 = lift1 f b1 $* b2

lift3 :: (a-> b -> c -> d) -> (Behavior a-> Behavior b -> Behavior c -> Behavior d)
lift3 f b1 b2 b3 = lift2 f b1 b2 $* b3

fstB = lift1 fst
sndB = lift1 snd

pairB = lift2 ((,))
--
--  Conditional operators, whic can be used with when and while
--
(>*),(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

startsWith :: Behavior a -> Behavior a
startsWith (Behavior fa) =
    memoB $ Behavior (\uts@(us, ts) -> loop us ts (fa uts) )
     where loop (_:us) (_:ts) ~(a:as)  = a:repeat a

-- untilB b1 b2 -------------------
-- starts with behavior b1 and change to b2 
-- evaluating functions fb and fe, we get b1 and fb'
-- Later when we evalueate fb', we get b2
-- Notice that fe has no interest in values of us and ts
--  This is because us and ts are used by the animation part
--  event procesing do not use that. Here it is used just to
--  pass to next loop call. Check  'where loop (_:us) (_:ts)' ... line
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `untilB` Event fe =
    memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
     where loop (_:us) (_:ts) ~(e:es) (b:bs) = b:bbs
              where bbs = case e of
                     Nothing -> loop us ts es bs
                     Just (Behavior fb') -> fb' (us, ts)

switch :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `switch` Event fe =
    memoB $ Behavior (\uts@(us, ts) -> loop us ts (fe uts) (fb uts))
     where loop (_:us) (_:ts) ~(e:es) (b:bs) = b:bbs
            where bbs = case e of 
                    Nothing -> loop us ts es bs
                    Just (Behavior fb') -> loop us ts es (fb' (us,ts))

--
-- modulate gives output stream with values in first behavior. when event happends, 
-- this moduates (modifies) stream with value in second behavior in event
-- This is similar to switch, where value change to value in behavior when event
-- happens, but switch back to original after the event
modulate :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `modulate` Event fe =
    memoB $ Behavior (\uts@(us, ts) -> loop us ts (fe uts) (fb uts))
     where loop (_:us) (_:ts) ~(e:es) (b:bs) = b:bbs
            where bbs = case e of 
                    Nothing -> loop us ts es  (fb (us,ts))
                    Just (Behavior fb') -> loop us ts es (fb' (us,ts))
-- snapshot
-- snapshot can be used if you want calculate a value based on previous value
-- snapshot combines current value in event and current value in behavior
-- and produce a pair. You can apply this pair to a function using =>>
snapshot :: Event a -> Behavior b -> Event (a,b)
Event fe `snapshot` Behavior fb
    = Event (\uts -> zipWith aux (fe uts) (fb uts) )
        where aux (Just x) y = Just (x,y)
              aux Nothing _ = Nothing
--
-- This version of snapshot converts Event (a,b) into Event b, by 
-- applying (a,b) to function snd
snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd

-- step starts with a and switch to values in the events
step :: a -> Event a -> Behavior a
a `step` e = constB a `switch` e =>> constB

-- stepAccum starts with a and then accumulates using
--  function in the Event
stepAccum :: a -> Event (a->a) -> Behavior a
a `stepAccum` e = b
    where b = a `step` (e `snapshot` b =>> uncurry ($))

-- withElem
-- 
withElem :: Event a -> [b] -> Event (a,b)
withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
    where
        loop (Just a:evs) (b:bs) = Just (a,b):loop evs bs
        loop (Nothing:evs) bs = Nothing:loop evs bs
--
--
withElem_ :: Event a ->[b]-> Event b
withElem_ e bs = e `withElem` bs =>> snd
-- popular events --------------------------
-- Left Button Pressed


--  mouse pressed event processing. f is a function to filter event futher
buttonPressed :: (Loc->Bool->Bool->Bool) -> Event Loc
buttonPressed f = Event (\(uas,_) -> map getbp uas)
    where getbp (Just (MouseButton p left press)) | (f p left press) = Just p
                                                  | otherwise = Nothing
          getbp _ = Nothing

-- is left button Pressed?
lbp = buttonPressed (\_ left press -> left && press)

-- is left button pressed inside rect
lbpInRect p1 p2 = buttonPressed f where
    f p left press = left && press && (isInRect p p1 p2)

-- Key Pressed
-- notice that the Event (which is a fucntion) do not have any interest in time 
-- part ts. So ts is ignored using _
key :: Event Char
key = Event (\(uas,_) -> map getkey uas)
       where getkey (Just (Key ch True)) = Just ch
             getkey _ = Nothing

keyPressed :: Char -> Event Char
keyPressed c = Event (\(uas,_) -> map getkey uas)
       where getkey (Just (Key c True)) = Just c
             getkey _ = Nothing

             
keyRel :: Char -> Event Char
keyRel c = Event (\(uas,_) -> map getkey uas)
       where getkey (Just (Key ch False)) | ch==c = Just c
                                          | otherwise = Nothing
             getkey _ = Nothing

arrow :: Event KeyType
arrow = Event (\(uas,_) -> map getkey uas)
    where getkey (Just (Arrow d)) = Just d
          getkey _ = Nothing            

whenArrow :: UserEvent.KeyType -> Event KeyType
whenArrow dt = Event (\(uas,_) -> map getkey uas)
    where getkey (Just (Arrow d)) | dt == d   = Just dt
                                  | otherwise = Nothing
          getkey _ = Nothing 

-- Mouse Motion
mm :: Event Loc
mm = Event (\(uas, _) -> map getmm uas)
    where getmm (Just (MouseMove pt)) = Just pt
          getmm _ = Nothing

mouse :: (Behavior Float, Behavior Float) 
mouse = (fstB m, sndB m)
    where m = (0,0) `step` mm
-- 
-- event oprerators ----------------------
Event fe =>> f = Event (map (fmap f).fe)   

e ->> v = e =>> \_ -> v

while :: Behavior Bool -> Event ()
while (Behavior fb)
   = Event (\uts -> map aux (fb uts))
     where aux True = Just ()
           aux False = Nothing

unique :: Eq a => Event a -> Event a
unique (Event fe) = Event (\uts -> aux (fe uts))
    where aux xs = zipWith remdup (Nothing:xs) xs
          remdup x y | x == y = Nothing
                     | otherwise = y

whenB :: Behavior Bool -> Event ()
whenB = unique.while

--
-- OR condition on events.
-- (key .|. mouse) produce either key or mouse, whichever happend
(.|.) :: Event a -> Event a -> Event a
Event fe1 .|. Event fe2 = 
    Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
      where aux Nothing Nothing = Nothing
            aux (Just x) _ = Just x
            aux _ (Just y) = Just y

            
-- MEMO Implementation ----------
memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)

memo1 :: (a->b)-> (a->b)
memo1 f = unsafePerformIO $ do
    cache <- newIORef []
    return $ \x -> unsafePerformIO $ do
        vals <- readIORef cache
        case x `inCache` vals of
            Nothing -> do let y = f x
                          writeIORef cache [(x,y)]
                          return y
            Just y -> do return y
inCache :: a -> [(a,b)] -> Maybe b
x `inCache` [] = Nothing
x `inCache` ((x',y'):xys) = 
    if unsafePtrEq x x' then Just y'
    else x `inCache` xys
unsafePtrEq x y = case reallyUnsafePtrEquality# x y of
    1# -> True
    _ -> False
