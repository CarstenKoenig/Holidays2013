module Main where

import Codec.Picture (Pixel8, PixelRGB8(..), Image, generateImage, writePng)

data Compl =
    C { re :: Double
      , im :: Double
      } deriving (Show, Eq)

data ViewWindow =
    View { upperLeft  :: Compl
         , lowerRight :: Compl
         } deriving (Show)

data PictureCoords = Coords { x :: Int, y :: Int }
data PictureSize   = Size   { width :: Int, height :: Int }

type Steps = Int

main :: IO()
main = do
    putStrLn "generating Mandelbrot..."
    let view       = View (C (-2.4) 1.2) (C 1.1 (-1.4))
    let maxSteps   = 2048
    let resolution = Size 1000 700
    let image      = mandelbrotImage maxSteps view resolution

    writePng "./mandelbrot.png" image

    putStrLn "you should have a image (mandelbrot.png)"

mandelbrotImage :: Steps -> ViewWindow -> PictureSize -> Image PixelRGB8
mandelbrotImage maxSteps vw sz@(Size w h) = generateImage calcPixel w h
    where calcPixel x' y' = color maxSteps $ mandelbrotIter maxSteps $ project vw sz (Coords x' y')

color :: Steps -> Steps -> PixelRGB8
color maxSteps steps =
    PixelRGB8 (fromIntegral sr) (fromIntegral sg) (fromIntegral sb)
    where s  = steps * 256*256*256 `div` maxSteps 
          sr = s
          sg = s `div` 256
          sb = s `div` 65536

-- | translates a picture-coords into the ViewWindow
project :: ViewWindow -> PictureSize -> PictureCoords -> Compl
project (View ul lr) sz c = ul + (C (w*x') (h*y'))
    where (C w h) = lr - ul
          x'      = fromIntegral (x c) / fromIntegral (width sz)
          y'      = fromIntegral (y c) / fromIntegral (height sz)

-- | processes - based on a maximum step count and a starting point -
--   the numbers or steps it takes a point using the iteration-rule
--   z' = z*z + c - to escape the bound region
mandelbrotIter :: Steps -> Compl -> Steps
mandelbrotIter maxSteps c = runIter 0 c
    where runIter steps z =
            if steps >= maxSteps || escapes z
                then steps
                else runIter (steps+1) (iter z)
          iter z = z*z + c

-- | does a point in the complex plane escape to infinity
--   (of course using mandelbrots iteration rule z' = z*z + c)
escapes :: Compl -> Bool
escapes = (>= 4) . len2

len2 :: Compl -> Double
len2 (C r i) = r*r + i*i

rgb :: (Pixel8, Pixel8, Pixel8) -> PixelRGB8
rgb (r, g, b) = PixelRGB8 r g b

-- | Implement Compl as Num represented as a complex number in the
--   most obvious way - notice that signum was choosen to fullfill
--   abs x * signum x == x
instance Num Compl where
    fromInteger i = C (fromInteger i) 0
    (C r i) + (C r' i') = C (r+r') (i+i')
    (C r i) - (C r' i') = C (r-r') (i-i')
    (C r i) * (C r' i') = C (r*r' - i*i') (r*i' + i*r')
    negate (C r i)      = C (-r) (-i)
    abs c               = C (sqrt $ len2 c) 0
    signum c@(C r i)    = C (r / l2) (i / l2)
        where l2        = len2 c