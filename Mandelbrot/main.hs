module Main where

import Mandelbrot

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
Buttons
------------------------------------------------------------------------------}

main :: IO ()
main = do
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just "./"
        } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Mandelbrot generator "
    -- UI.addStyleSheet w "buttons.css"

    message   <- string ""
    image     <- mandelbrotDisplay

    getBody w #+
        [UI.div #. "wrap" #+ [element message, element image]]

    renderDisplay 5000 message image


mkImage :: String -> UI Element
mkImage source = do
    image <- UI.image #. "mandelImage"
    element image # set Attr.src source

data MandelbrotDisplay = MandelbrotDisplay
    { view       :: ViewWindow
    , resolution :: PictureSize
    , visual     :: Element
    }

mandelbrotDisplay :: UI MandelbrotDisplay
mandelbrotDisplay = do
    img <- mkImage ""
    return $ MandelbrotDisplay defaultView defaultRes img
    where defaultView = View (C (-2.4) 1.2) (C 1.1 (-1.4))
          defaultRes  = Size 1024 711

renderDisplay :: Steps -> Element -> MandelbrotDisplay -> UI ()
renderDisplay maxSteps message d = do
    element (visual d) # set Attr.src ""
    element message # set text "rendering please wait ..."
    liftIO $ createImageParallel (view d) maxSteps (resolution d) "./mandelbrot.png"
    element message # set text ""
    element (visual d) # set Attr.src "/static/mandelbrot.png"
    return ()

instance Widget MandelbrotDisplay where
    getElement = visual
