module Main where

import Mandelbrot

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import qualified Graphics.UI.Threepenny.Events as Ev
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

    md <- mandelbrotDisplay
    on Ev.mousedown (image md) $ \ (x,y) -> renderDisplay (zoomIn md 2 (Coords x y))

    getBody w #+ [element md]

    renderDisplay md


mkImage :: String -> UI Element
mkImage source = do
    img <- UI.image #. "mandelImage"
    element img # set Attr.src source

data MandelbrotDisplay = MandelbrotDisplay
    { view       :: ViewWindow
    , resolution :: PictureSize
    , maxSteps   :: Steps
    , visual     :: Element
    , image      :: Element
    , message    :: Element
    }

mandelbrotDisplay :: UI MandelbrotDisplay
mandelbrotDisplay = do
    img <- mkImage ""
    msg <- string ""
    d <- UI.div #. "wrap" #+ [element msg, element img]
    return $ MandelbrotDisplay defaultView defaultRes defaultSteps d img msg
    where defaultView  = View (C (-2.4) 1.2) (C 1.1 (-1.4))
          defaultRes   = Size 1024 711
          defaultSteps = 5000

renderDisplay :: MandelbrotDisplay -> UI ()
renderDisplay d = do
    element (image d) # set Attr.src ""
    element (message d) # set text "rendering please wait ..."
    liftIO $ createImageParallel (view d) (maxSteps d) (resolution d) "./mandelbrot.png"
    element (message d) # set text ""
    element (image d) # set Attr.src "/static/mandelbrot.png"
    return ()

zoomIn :: MandelbrotDisplay -> Double -> PictureCoords -> MandelbrotDisplay 
zoomIn d z c = d { view = view' }
    where cent = project (view d) (resolution d) c
          view' = zoomTo z cent $ view d

instance Widget MandelbrotDisplay where
    getElement = visual
