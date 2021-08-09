
module Hudak.Util where
import qualified Data.Text as T
import Text.Printf

strToTxt = T.pack
floatToStr::Float->String
floatToStr n = printf "%08.4f" n -- total width 3 chars, 2 decimal places

floatToTxt n = (T.pack.floatToStr) n

intToStr::Int->String
intToStr n = printf "%d" n -- total width 4

-- is y between a and b
between y a b = (y > a) && (y < b)

-- is a point inside a rectangle
isInRect (x,y) (x1,y1) (x2,y2) =
    let c1 = between x x1 x2
        c2 = between y y1 y2
    in c1 && c2

toRect (x,y) aSize = ((x,y), (x+aSize, y+aSize))             
-- is a from y at distance d?
isArround a d y = between (y-d) (y+d) a