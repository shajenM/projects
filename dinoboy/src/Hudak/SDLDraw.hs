module SDLDraw where

--import qualified SOE as SOE
import qualified SDL
import qualified SDL.Mixer as Mix
import qualified SDL.Font as SDLFONT
import qualified SDL.Image as SDLIMG
import SDL.Vect
import qualified SDL.Primitive
import Foreign.C.Types        (CInt)
import Data.Word (Word32)
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad      (when)

import Shape
import Picture
import Region
import GHC.Int
import Util

-- SDL graphics geometry.
-- in Pixels

--   0,0 ----------------------- 600,0
--    |                             |
--    |                             |
--    |                             |
--    |            300,250          |
--    |                             |
--    |                             |
--    |                             |
--   0,500 --------------------- 600,500

-- User screen geometry.
-- in Inches
-- -3.0,-2.5 ------------------- +3.0,-2.5
--     |                             |
--     |                             |
--     |                             |
--     |             0,0             |
--     |                             |
--     |                             |
--     |                             |
-- -3.0,+2.5 ------------------- +3.0,+2.5

type Pix = Int32
newtype SDLGraphic = SDLGraphic (SDLEnv ->IO ())
type SDLResource = SDLEnv ->IO SDLEnv
-- renderer type
-- Hardware based or Software based. HardwareBased not working
--   with WSL2 and Xming.exe
sdlRendererType = SDL.RendererConfig {
     SDL.rendererType = SDL.SoftwareRenderer,
     SDL.rendererTargetTexture = False 
    }
--
-- common properties
--
data SDLEnv = SDLEnv {
  renderer  ::SDL.Renderer,  
  fonts     ::M.Map Int SDLFONT.Font,
  images    ::M.Map T.Text SDL.Texture,
  audios    ::M.Map T.Text Mix.Chunk
}

type Coordinate = (Pix, Pix)  -- coordinate in pixels


--  SDL.Primitve use CInt type.
--  SDL2 mouse locations use Int32 type
--  So here I am using Int32 as Pixel type and for Primitives, I will convert to CInt
-- ch 4.2
xWin, yWin  :: Pix
xWin    = 1000
yWin    = 500

xWinCInt::CInt
xWinCInt = fromIntegral xWin
yWinCInt ::CInt
yWinCInt = fromIntegral yWin

sdlPeach :: SDL.Primitive.Color
sdlPeach = V4 255 (255-40) (255-50) 255

sdlBlue :: SDL.Primitive.Color
sdlBlue = V4 0 0 255 255

sdlGreen :: SDL.Primitive.Color
sdlGreen = V4 0 255 0 255

sdlYellow :: SDL.Primitive.Color
sdlYellow = V4 255 255 0 255

sdlRed :: SDL.Primitive.Color
sdlRed = V4 255 0 0 255

sdlBlack :: SDL.Primitive.Color
sdlBlack = V4 0 0 0 255

sdlColor :: Color -> SDL.Primitive.Color
sdlColor Peach = sdlPeach
sdlColor Blue  = sdlBlue
sdlColor Yellow = sdlYellow
sdlColor Green  = sdlGreen
sdlColor Red    = sdlRed
sdlColor Black  = sdlBlack
sdlColor TransBlack = V4 0 0 0 20
sdlColor Brown  = V4 160 82 45 255
sdlColor Magenta = V4 255 0 255 255
sdlColor White = V4 255 255 255 255
sdlColor Aquamarine = V4 102 255 170 200
sdlColor PersianIndigo = V4 53 28 117 255
sdlColor Gold = V4 255 215 0 255
sdlColor GoldD = V4 176 143 38 255
sdlColor GoldL = V4 232 185 35 255


-- The last number is the transperancy number. 0 means total transparant
--   255 means opaque. This can be used for glass or bubbles
sdlColor (RGBColor a b c) = V4 (fromIntegral a) (fromIntegral b) (fromIntegral c) 255
sdlColor _ = sdlRed

addSDLColor :: SDL.Primitive.Color -> SDL.Primitive.Color ->SDL.Primitive.Color
addSDLColor c1 c2 = c1+c2

addColor:: Color -> Color -> Color
addColor c1 c2 = RGBColor a b c where
    (V4 x y z k) = addSDLColor (sdlColor c1) (sdlColor c2)
    a = fromIntegral x
    b = fromIntegral y
    c = fromIntegral z

trans   :: USize -> SDL.Primitive.Pos
trans (x,y) = V2 (fromIntegral (xWin2 + inchToPixel x)) (fromIntegral (yWin2 + inchToPixel y))

xWin2, yWin2    ::Pix
xWin2   = xWin `div` 2
yWin2   = yWin `div` 2

transList   :: [USize] -> [SDL.Primitive.Pos]
transList [] = []
transList (p:ps) = trans p : transList ps

inchToPixel :: Float -> Pix
inchToPixel x = round (100*x)

pixelToInch :: Pix -> Float
pixelToInch n = int32ToFloat n/100

-- graphics coordinate to user screen location (inches)
coordToLoc::Coordinate -> Loc
coordToLoc (x,y) = (pixelToInch(x), pixelToInch(y))

-- user screen location to graphics location in pixels
locToCoord :: Loc -> Coordinate
locToCoord (x,y) = ( inchToPixel(x), inchToPixel(y))

intToCInt:: Int -> CInt
intToCInt x = fromIntegral x

int32ToCInt:: Int32 -> CInt
int32ToCInt x = fromIntegral x

floatToCInt:: Float -> CInt
floatToCInt x = round x

--word32ToInt :: Word32 -> Int
--word32ToInt = fromIntegral

word32ToInt32 :: Word32 -> Int32
word32ToInt32 = fromIntegral

--intToFloat :: Int -> Float
--intToFloat n = fromInteger (toInteger n)

int32ToFloat :: Int32 -> Float
int32ToFloat n = fromInteger (toInteger n)

-- SDL.time gives time in seconds
-- converted to micro seconds
timeGetTime :: IO Word32
timeGetTime = do
  timeInSec <- SDL.time
  return $ round $ timeInSec * 1000

rad t = (fromIntegral t)*pi/180
fx t = ff t sin
fy t = ff t cos
ff t f =  abs (round ((f (rad t))*100))
adjust t =  abs (round (t*100))
---------------------

-- convert resource into SDLResource
resourceToSDLResource :: Resource -> SDLResource
resourceToSDLResource (Font fontSize) = sdlLoadFont fontSize
resourceToSDLResource (BoyImage name) = sdlLoadImage name
resourceToSDLResource (BackGround name) = sdlLoadImage name
resourceToSDLResource (Audio name) = sdlLoadAudio name
  
-- Convert user shape into SDL shapes
shapeToGraphic :: Shape -> SDLGraphic
shapeToGraphic s  = shapeAtToGraphic 0 0 s Peach

shapeAtToGraphic :: Float -> Float -> Shape -> Color -> SDLGraphic
shapeAtToGraphic x y (Ellipse r1 r2) c = 
  SDLGraphic (sdlEllipse (trans(x, y)) (inchToPixel r1) (inchToPixel r2) (sdlColor c) )

shapeAtToGraphic x y (Text font str) c = 
  SDLGraphic (sdlText tx ty font str (sdlColor c) ) where
    (V2 tx ty)= trans (x,y)

shapeAtToGraphic x y (SpriteImage res n flip) _ = 
  SDLGraphic (sdlSpriteImage tx ty res cn flip) where
    (V2 tx ty)= trans (x,y)
    cn = intToCInt n

shapeAtToGraphic x y (BackgroundImage res skin) _ = 
  SDLGraphic (sdlBgImage tx ty res cn) where
    (V2 tx ty)= trans (x,y)
    cn = intToCInt skin

shapeAtToGraphic x y (Rectangle w h) c = 
  SDLGraphic (sdlRect p1 p2 (sdlColor c)) where
    p1 = trans(x,y)
    p2 = trans (x+w,y+h)

shapeAtToGraphic x y (RectangleFill w h) c = 
  SDLGraphic (sdlRectFill p1 p2 (sdlColor c)) where
    p1 = trans(x,y)
    p2 = trans (x+w,y+h)

---------------
sdlRect p1 p2 c e = SDL.Primitive.rectangle (renderer e) p1 p2 c
sdlRectFill p1 p2 c e = SDL.Primitive.fillRectangle (renderer e) p1 p2 c

--sdlEllipse::SDL.Primitive.Pos -> Pix -> Pix-> SDL.Primitive.Color -> SDLGraphic
sdlEllipse p r1 r2 c e = 
  SDL.Primitive.fillEllipse (renderer e) p (int32ToCInt r1) (int32ToCInt r2) c

---------------------------
-- How to convert Region to SDL types
-- regionAtGraphic :: Vector -> Region -> SDLGraphic
regionAtGraphic :: Loc -> Color -> Region -> SDLGraphic
regionAtGraphic (x,y) c (Region s) = shapeAtToGraphic x y s c

-- Translating an ecclipse means move it to a different location
regionAtGraphic (x,y) c (Translate (u,v) r) = regionAtGraphic (x+u, y+v) c r

-- scaling an eclipse means increase( times)  its radious  by (u,v)
-- TBD below is wrong , need to be corrected
regionAtGraphic (x,y) c (Scale (u,v) r) = regionAtGraphic (x*u, y*v) c  r

regionToGraphic :: Region -> SDLGraphic
regionToGraphic r = regionAtGraphic (0,0) Peach r

regionToSound :: Region -> SDLGraphic
regionToSound (Region (Sound (Audio res))) = SDLGraphic (sdlSound res)
regionToSound Empty = SDLGraphic sdlNoSound
---------------------------
-- How to convert Picture to SDL 
picToGraphic :: Picture -> SDLGraphic
picToGraphic (Picture c r) = regionAtGraphic (0,0) c r
picToGraphic (p1 `Over` p2) = picToGraphic p1  `sdlOver` picToGraphic p2
picToGraphic EmptyPic = shapeToGraphic (Rectangle 0.0 0.0)
picToGraphic (SoundP r) = regionToSound r

sdlOver :: SDLGraphic -> SDLGraphic -> SDLGraphic
sdlOver (SDLGraphic f1) (SDLGraphic f2) = SDLGraphic fo
    where fo r = do
                    f1 r
                    f2 r
--
-- Play Sound
sdlNoSound env = return ()
sdlSound name env = do
    case M.lookup name (audios env) of
        Nothing -> do
          print $ "ERROR: audio not loaded !!!" ++ (show name)
          return ()
        (Just t_sound) -> 
          -- Mix.playMusic Mix.Once t_sound  -- for music
          Mix.play t_sound  -- for Chunk

-- load audio
sdlLoadAudio name env = do
    let file=(T.unpack name)
    print $ "loading sound "++file
    aud <- Mix.load (path++file)
    let updatedAudios = M.insert name aud (audios env)
    return env { audios = updatedAudios}

------------------
-- this is used by mouse motion event
--  first we need to convert (0,0) to center of screen
gPtToPt :: Coordinate -> Coordinate
gPtToPt (x,y) = (x - xWin2, y - yWin2)
-----------------------------
----------------------------
-- Image Functions
--
sdlLoadImage name env = do
    img <- loadImage (T.unpack name) (renderer env)
    let updatedImages = M.insert name img (images env)
    return env { images = updatedImages}

--
loadImage m r = do
  print $ "loading image "++m
  img <- SDLIMG.load (path++m)
  texture_img <- SDL.createTextureFromSurface r img
  SDL.freeSurface img
  tq <- SDL.queryTexture texture_img
  let tw = SDL.textureWidth tq
      th = SDL.textureHeight tq
  print $ "width "++ (show tw) ++ " height "++ (show th)
  return texture_img

-- render image

sdlBgImage x y (BackGround name) n env = do
  case M.lookup name (images env) of
    Nothing -> do
      print $ "ERROR: Background Image not loaded !!!" ++ (show name)
      return ()
    (Just t_bg) -> sdlDrawBgImage x y n (renderer env) t_bg

sdlDrawBgImage x y n r t_bg = do
  -- n is nth image starting from 0 to 5
  --  bg image size is 1500 x 1000 (wxh)
  tq <- SDL.queryTexture t_bg
  let bgW = SDL.textureWidth tq
      bgH = SDL.textureHeight tq

  let sk = n `mod` bgW
      remW = (bgW-sk)
      src1P = sk
      src1W = min remW  xWinCInt
      dst2P = src1W
  let src1  = Just (SDL.Rectangle (P (V2 src1P 0)) (V2 src1W bgH))
  let dest1 = Just (SDL.Rectangle (P (V2 0 0)) (V2 src1W yWinCInt))
  SDL.copy r t_bg src1 dest1 

  if (src1W < xWinCInt) then  do
      let src2  = Just (SDL.Rectangle (P (V2 0 0)) (V2 xWinCInt bgH))
          dest2 = Just (SDL.Rectangle (P (V2 dst2P 0)) (V2 xWinCInt yWinCInt)) 
      SDL.copy r t_bg src2 dest2
  else return ()


-- n is which image inside 
sdlSpriteImage x y (BoyImage name) n flip env = do
    case M.lookup name (images env) of
      Nothing -> do
        print "ERROR: Boy Image not loaded !!!"
        return ()
      (Just t_sprite) -> sdlDrawSpriteImage x y n flip t_sprite (renderer env)

sdlDrawSpriteImage x y n flip t_sprite r = do
  -- n is nth image starting from 0 to 5
  let tw = 233
      th = 233
  tq <- SDL.queryTexture t_sprite
  let imgWidth = SDL.textureWidth tq
      imgHeight = SDL.textureHeight tq

  let scaleX = 100
      scaleY = 100
  let dw = tw - scaleX
      dh = th - scaleY
      num_imgs = imgWidth `div` tw

  let spriteNum = n `mod` num_imgs
  let angle = 0
  let center = Nothing
  let src  = Just (SDL.Rectangle (P (V2 (n*tw) 0)) (V2 tw th))
            -- source rectangle: from the texture which part to take
  let dest = Just (SDL.Rectangle (P (V2 x y)) (V2 dw dh))
            -- destination rectangle: where on screen how much area of src to display
  SDL.copyEx r t_sprite src dest angle center (V2 flip False)
  --------------------------------------
  -- Font functions
  --
path = "res/"
sans = path++"OpenSans-Regular.ttf"

sdlLoadFont fontSize env = do
    f <- loadFont fontSize
    let updatedFonts = M.insert fontSize f (fonts env)
    return env { fonts = updatedFonts}

-- fontSize = 50   -- 80 is good for collision message
loadFont fontSize = do
    print $ "loading font " ++ (show fontSize)
    font <- SDLFONT.load sans fontSize
    return font

sdlText x y (Font f) str color env = 
  case M.lookup f (fonts env) of
      Nothing -> do
        print $ "ERROR: font "++(show f)++" not loaded !!!"
        return ()
      (Just tFont) -> sdlDrawText x y tFont str color env

sdlDrawText x y f str color env = do
    t_surface <- SDLFONT.blended f color str 
    t_texture <- SDL.createTextureFromSurface (renderer env) t_surface
    SDL.freeSurface t_surface
    tq <- SDL.queryTexture t_texture
    let tw = SDL.textureWidth tq
        th = SDL.textureHeight tq
    -- print ("tw ="++show tw ++" th="++ show th)
    let scaleX = 1
        scaleY = 1
    let dw = tw *scaleX
        dh = th *scaleY
    let src  = Just (SDL.Rectangle (P (V2 0 0)) (V2 tw th))
              -- source rectangle: from the texture which part to take
    let dest = Just (SDL.Rectangle (P (V2 x y)) (V2 dw dh))
              -- destination rectangle: where on screen how much area of src to display
    SDL.copy (renderer env) t_texture src dest 
    SDL.destroyTexture t_texture
