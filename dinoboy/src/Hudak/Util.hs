
module Util where
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

-- is a from y at distance d?
isArround a d y = between (y-d) (y+d) a