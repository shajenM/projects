module Hudak.UserEvent 
    -- (UserEvent (Button, Key, Arrow, KeyType(...), MouseMove,Resize, Refresh, Closed, Idle, Tick),)
    where

import Hudak.Shape

data UserEvent = 
      Key { char :: Char, isDown :: Bool }
    | Arrow KeyType
    | MouseButton { pt :: Loc, isLeft :: Bool, isDown :: Bool }
    | MouseMove { pt :: Loc }
    | Resize USize
    | Tick
    | Refresh
    | Closed
    | Idle
        deriving Show

data KeyType = 
      Left
    | Right
    | Up
    | Down
    deriving (Eq, Show)