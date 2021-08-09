module Hudak.Picture (
    Picture (Picture, SoundP, Over, EmptyPic),
    --Color (Black, Blue, Green, Cyan, Red, Magenta, Yellow, White, Peach, Brown, Aquamarine, RGBColor),
    Color (..), -- export all colors
    module Hudak.Region
    ) where

import Hudak.Region

-- Chapter 10
data Picture = 
        Picture Color Region
    |   SoundP Region
    |   Picture `Over` Picture
    |   EmptyPic
    deriving Show

data Color = Black
    | TransBlack
    | Blue
    | Green
    | Cyan
    | Red
    | Magenta
    | Yellow
    | White
    | Peach
    | Brown
    | Aquamarine
    | PersianIndigo
    | Gold
    | GoldD
    | GoldL
    | RGBColor Int Int Int
    deriving (Eq, Ord, Show)

