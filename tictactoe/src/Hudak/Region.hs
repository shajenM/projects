module Hudak.Region (
    Region(Region, Translate, Scale, Complement,
    Union, Intersect, Empty),
    containsS, containsR,
    module Hudak.Shape
    ) where
import Hudak.Shape

-- Chapter 8.2.4
data Region = Region Shape
    | Translate USize Region
    | Scale USize Region
    | Complement Region
    | Region `Union` Region
    | Region `Intersect` Region
    | Empty
    deriving Show

--
-- check Containment of Shapes. returns Bool
containsS :: Shape -> Loc -> Bool
(Ellipse r1 r2) `containsS` (x,y) = (x/r1)^2 + (y/r2)^2 <= 1

-- Containment of Regions
containsR :: Region -> Loc -> Bool
(Region s) `containsR` p = s `containsS` p
(Translate (u,v) r) `containsR` (x,y) = r `containsR` (x-u, y-v)
(Scale (u,v) r) `containsR` (x,y) = r `containsR` (x/u, y/v)
(Complement r) `containsR` p = not (r `containsR` p)
Empty `containsR` p = False

isLeftOf :: Loc -> Ray -> Bool
(px,py) `isLeftOf` ((ax,ay), (bx,by))
    = let (s,t) = (px-ax, py-ay)
          (u,v) = (px-bx, py-by)
      in s*v >= t*u
type Ray = (Loc, Loc)
--


