{-# LANGUAGE OverloadedStrings #-}
module SDLAnim where

import qualified SDL
import qualified Data.Text as T
import Foreign.C.Types
import Control.Concurrent (threadDelay)
import Control.Monad        (when)

import qualified SDL.Primitive
import qualified SDL.Framerate
import SDL.Vect             (V2(..), V4(..))
import SDL                  (($=))


import SDLDraw

type Animation a = Time -> a
type Time = CInt

pos = Just (0,0)
size = Just (xWin, yWin)
freq = Just 30

black :: SDL.Primitive.Color
black = V4 0 0 0 255
win_type = SDL.defaultWindow
win_delay=2*1000000

(sw, sh) =(640,480)
ws = V2 sw sh

-- in animate, anim is a function which gives a SOE.Graphic
--  when invoked with argument time.

animate :: String -> Animation SDLGraphic -> IO ()
animate title anim =do
    SDL.initialize [SDL.InitVideo]

    w <- SDL.createWindow (T.pack title) win_type { SDL.windowInitialSize = ws }
    r <- SDL.createRenderer w (-1) SDL.defaultRenderer
    SDL.showWindow w
    let sdlenv = SDLEnv{renderer=r}
  
    let fps   = 60  -- How fast do we aim to render?
    let limit = 1000 -- How many frames will we render?
  
    SDL.Framerate.with fps $ loopFor limit sdlenv anim
    threadDelay win_delay
    SDL.destroyWindow w
    SDL.quit

loopFor :: CInt -> SDLEnv -> Animation SDLGraphic -> SDL.Framerate.Manager -> IO ()
loopFor limit env anim fpsm = loop'
    where
    loop' :: IO ()
    loop' = do

        -- How many frames have we drawn until now?
        frames <- fromIntegral `fmap` SDL.Framerate.count fpsm

        -- Clear the screen!
        SDL.rendererDrawColor (renderer env) $= black
        SDL.clear (renderer env)
        let (SDLGraphic f )=anim frames
        f env
        SDL.present (renderer env)
        SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.
        when (frames < limit) loop'
