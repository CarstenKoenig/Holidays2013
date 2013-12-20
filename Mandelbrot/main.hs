module Main where

import Codec.Picture (Pixel8, PixelRGB8(..), Image, generateImage, writePng)

main :: IO()
main = do
    putStrLn "Hello Mandelbrot..."
    writePng "./pictrue.png" $ rainbowScreen (256, 256)
    putStrLn "you should have a image (picture.png)"

rgb :: (Pixel8, Pixel8, Pixel8) -> PixelRGB8
rgb (r, g, b) = PixelRGB8 r g b

rainbowScreen :: (Int, Int) -> Image PixelRGB8
rainbowScreen (width, height) = generateImage rainbow width height
    where rainbow x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
