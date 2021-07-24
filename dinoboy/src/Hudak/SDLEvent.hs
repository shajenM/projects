module SDLEvent where

-- This module have the mapping between
-- SDL.Event and user defined events
import qualified SDL
import SDL.Vect   --  (Point(..), V2(..), V4(..))
import UserEvent
import SDLDraw
import Shape

-- 
-- I prepared this refering
-- https://hackage.haskell.org/package/sdl2-2.5.2.0/docs/SDL-Event.html#t:MouseButton
-- https://github.com/palf/haskell-sdl2-examples/blob/master/examples/lesson13/src/Lesson13.hs
--

sdlEventToUserAction :: SDL.EventPayload-> UserEvent
sdlEventToUserAction SDL.QuitEvent = Closed
sdlEventToUserAction (SDL.KeyboardEvent e) = sdlKeyToUser e
sdlEventToUserAction (SDL.MouseButtonEvent e) = sdlMouseButtonToUser e
sdlEventToUserAction (SDL.MouseMotionEvent e) = sdlMouseMotionToUser e
sdlEventToUserAction (SDL.WindowResizedEvent e) = sdlResizeToUser e
sdlEventToUserAction _ = Idle

--sdlKeyToUser (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
sdlKeyToUser (SDL.KeyboardEventData _ p _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Closed
    SDL.KeycodeW      -> Key 'w' pressState
    SDL.KeycodeS      -> Key 's' True
    SDL.KeycodeR      -> Key 'r' True
    SDL.KeycodeB      -> Key 'b' True
    SDL.KeycodeY      -> Key 'y' True
    SDL.KeycodeSpace  -> Key ' ' pressState
    SDL.KeycodeRight  -> Arrow UserEvent.Right
    SDL.KeycodeLeft   -> Arrow UserEvent.Left
    SDL.KeycodeUp     -> Arrow UserEvent.Up
    SDL.KeycodeDown   -> Arrow UserEvent.Down
    _                 -> Idle  
    where
      pressState = p==SDL.Pressed

-- MOUSE BUTTON PRESS
sdlMouseButtonToUser (SDL.MouseButtonEventData _ btnMotion _ btnKey _ (P (V2 x y)) ) =
   Button (coordToLoc(x,y)) (btnKey==SDL.ButtonLeft) (btnMotion==SDL.Pressed)

-- MOUSE MOTION
sdlMouseMotionToUser (SDL.MouseMotionEventData _ _ _ (P (V2 x y)) _ ) =
  MouseMove (coordToLoc (gPtToPt(x,y))) 

-- WINDOW SIZE
sdlResizeToUser (SDL.WindowResizedEventData _ (V2 x y) ) = Resize ((pixelToInch x), (pixelToInch y))

-- ==================================================================
-- This is to test if f:(a -> IO Graphic) works
uas = cycle [Nothing, Just (Button (0,0) True True), Nothing]
ts  = [1,2..]:: [Time]
