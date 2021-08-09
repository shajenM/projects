module Hudak.Shape 
       --(
        --Shape(Ellipse), circle, Radious, Vertex, Vector, Loc, RecSize,
        --)
where
import qualified Data.Text as T

type Time = Float

-- The meaurements in screen is done in inches, which is float ( not integer).
-- One inch represents 100 pixels. 0.5 inch is 50 pixels. Pixels are Int type
-- for example
--  center of screen is (0.0, 0.0)
--  Ellipse 0.2 0.2 means Ellipse with radious 0.2 inches ie (0.2*100) 20 pixels

type Radious = Float

-- a size in inches specified by user
type USize = (Float, Float)
type Length = Float
-- Loc represents a location on screen in inches. (corresponding thing in graphics pixel is Coordinate)
type Loc = (Float, Float)

type SkinNumber = Int
type Flip = Bool

data Shape = 
          Ellipse Radious Radious
        | Text Resource T.Text
        | SpriteImage Resource SkinNumber Flip 
          -- SpriteImage with resource and nth skin, Flip=False or Flip=True change dir
        | BackgroundImage Resource SkinNumber
        | Rectangle Length Length
        | RectangleFill Length Length
        | LoadImage T.Text
        | Sound Resource
        deriving Show
circle r = Ellipse r r
square n = Rectangle n n

data Resource = 
          Font Int
        | BoyImage T.Text
        | BackGround T.Text
        | Audio T.Text
        deriving Show
