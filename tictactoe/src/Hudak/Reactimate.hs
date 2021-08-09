module Hudak.Reactimate where

import Hudak.Fal
import Hudak.SDLDraw
import Hudak.Shape
import Hudak.SDLEvent
import Hudak.UserEvent
import Hudak.Util

-- from SDL package
import qualified SDL
import qualified SDL.Mixer as Mix
import Data.Default.Class (def)
import qualified SDL.Font as SDLFONT
import Foreign.C.Types
import qualified SDL.Framerate
import SDL.Vect (V2(..), V4(..))
import SDL (($=))

-- from prelude
import Control.Concurrent (threadDelay, newChan, getChanContents, writeChan)
import Control.Monad        (when)
import qualified Data.Text as T
import qualified Data.Map as M

-- Window properties
win_type = SDL.defaultWindow
win_size = V2 (int32ToCInt xWin) (int32ToCInt yWin)
win_delay = 2*1000000

-- |
-- | reactimate 
-- |
reactimate :: String -> Behavior a -> SDLResource -> (a -> SDLGraphic) -> IO ()
reactimate title franProg resources toGraphic = do
    SDL.initialize [SDL.InitVideo, SDL.InitAudio]
    SDLFONT.initialize

    Mix.initialize [Mix.InitMP3]
    Mix.openAudio def 256

    w <- SDL.createWindow (T.pack title) win_type { SDL.windowInitialSize = win_size }
    r <- SDL.createRenderer w (-1) sdlRendererType
 
    let env = SDLEnv { renderer=r, fonts = M.empty, images = M.empty, audios = M.empty }
    let fps   = 60  -- How fast do we aim to render?
    let limit = 1000 -- How many frames will we render?
    env2 <- resources env
    -- testEnv env2
  
    (user, addEvents) <- windowUser w
    addEvents

    SDL.showWindow w
    SDL.Framerate.with fps $ loopFor limit env2 franProg toGraphic (user, addEvents)
    print "Exiting"
    closeWindow w


loopFor :: CInt -> SDLEnv -> Behavior a -> (a -> SDLGraphic) -> (([Maybe UserAction], [Time]), IO () ) -> SDL.Framerate.Manager -> IO ()
loopFor limit env franProg toGraphic  (us, addEvents) fpsm = loop'
    where
    loop' :: IO ()
    loop' = do
        let r = (renderer env)
        let drawItem (Just p) =  do  
                frames <- fromIntegral `fmap` SDL.Framerate.count fpsm
                -- Clear the screen!
                SDL.rendererDrawColor r $= sdlBlack
                SDL.clear r
                let (SDLGraphic drawFunc) = toGraphic p
                drawFunc env
                SDL.present r
                addEvents  -- addEvent is called to fill any pending events from pollEvent
                SDL.Framerate.delay_ fpsm -- Delay to keep frame rate constant.

            drawItem Nothing = do
                return ()

        mapM_ drawItem (runEvent (sample `snapshot_` franProg) us)
        {--
        ==The mapM_ is same as below code using mainLoop==
        let mainLoop (x:xs) = do 
                drawItem x
                -- addEvents
                mainLoop xs
            mainLoop [] = return ()
        mainLoop (runEvent (sample `snapshot_` franProg) us)
        --}
    -- Note that loop' is a infinite loop even though it is not called recursively.
    -- It is infinite because mapM_ operate on list user, which is infinite
-- --------------------------------------------------------------
runEvent :: Event a -> ([Maybe UserAction], [Time]) -> [Maybe a]
runEvent (Event fe) us = fe us

-- sample --
--    sample is just like lbp or key condition. It is looking for a delimter 'Nothing' in 'us'.
--    when found, it converts 'Event Nothing' into 'Event Just ()'
--    this monitors presence of events in us
--    When an event is present, runEvent (sample `snapshot_` prog) will not run prog
--      it will run only at times thre are no events
--   this version is different from original in book.
--   Here I added a check for Closed Event and break the loop
sample ::Event ()
sample = Event fe where
    fe ((Just Closed):us,ts) =  []
    fe ((Nothing):us,ts) =  Just (): fe (us,ts)
    fe ((Just _):us,ts) =  Nothing: fe (us,ts)

-- windowuser --
-- windowuser is just like makestream which returns two streams us and ts
--   and a function addEvent.
-- us : will have the UserEvent objects
-- ts : has the time :float
-- addEvent : functiion can be used to fill all the pending events into us stream.
--            addEvent is not infinite loop. It reads from pollEvent as long as events
--            present. It stops adding on Nothing, with last item as (Nothing, rt)
--            Nothing is ts act like a delimiter after a series of events.
--            `sample` is used to check this delimiter in us

windowUser ::SDL.Window -> IO (([Maybe UserAction], [Time]), IO () )
windowUser w = do
    (evs, addEv, ch) <- makeStream
    t0 <- timeGetTime

    let loop rt = do  -- loop rt s used by addEvents
            mev <- SDL.pollEvent
            -- print " event Happened"
            case mev of 
                Nothing -> return ()
                Just e -> do
                    -- note that sdlEventToUserAction converts SDLQuit into Closed
                    --    which is checked inside sample funcion which breaks the loop
                    let evtPayload = (sdlEventToUserAction.SDL.eventPayload) e
                    addEv (Just evtPayload, rt)
                    loop rt
    let addEvents = do  -- addEvents is used in last statement of windowUser
        t <- timeGetTime
        let rt = w32ToTime (t-t0)
        loop rt
        addEv (Nothing, rt)
    return ((map fst evs, map snd evs), addEvents)

-- t s in microseconds. convert to seconds
w32ToTime t = int32ToFloat (word32ToInt32 t)/1000
-- make stream
-- makeStream :: IO ([a], a -> IO ())
makeStream = 
    do ch <- newChan
       contents <- getChanContents ch
       return (contents, writeChan ch, ch)

closeWindow w = do       
    print "------- CLOSING WINDOW -----------"
      -- close device
    Mix.closeAudio
    -- quit
    Mix.quit
    SDL.destroyWindow w
    SDL.quit

