module Main where

import MandelbrotWidget

import Control.Monad

import Graphics.UI.Threepenny.Core

main :: IO ()
main = do

    let settings = defaultSettings

    -- create the start-image if neccessary
    verifyStartImageFile settings

    putStrLn "starting local server..."
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just "./"
        } (setup settings)

setup :: Settings -> Window -> UI ()
setup settings w = void $ do
        return w # set title "Mandelbrot generator "
        md <- mandelbrotDisplay settings
        getBody w #+ [element md]