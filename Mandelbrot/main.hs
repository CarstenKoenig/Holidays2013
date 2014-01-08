module Main where

import Mandelbrot

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import qualified Graphics.UI.Threepenny.Events as Ev

import Graphics.UI.Threepenny.Core

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

    (md, start) <- mandelbrotDisplay defaultView defaultRes defaultSteps defaultZoom
    getBody w #+ [element md]
    start ()

    where defaultView    = View (C (-2.4) 1.2) (C 1.1 (-1.4))
          defaultRes     = Size 1024 711
          defaultSteps   = 1024
          defaultZoom    = 4

data MandelbrotDisplay = MandelbrotDisplay
    { view       :: Behavior ViewWindow
    , visual     :: Element
    }


instance Widget MandelbrotDisplay where
    getElement = visual    

mandelbrotDisplay :: ViewWindow -> PictureSize -> Steps -> Double -> UI (MandelbrotDisplay, () -> UI ())
mandelbrotDisplay startView res maxSteps zoomF = do
    img <- mkImage ""
    msg <- string ""
    d <- UI.div #. "wrap" #+ [element msg, element img]

    let mdEvs = Ev.mousedown img
    let changeViewEvs = makeViewChange <$> mdEvs
    vw <- accumB startView changeViewEvs

    onChanges vw $ renderImage msg img
    let render() = renderImage msg img startView

    return $ (MandelbrotDisplay vw d, render)

    where makeViewChange (ptX, ptY) vw =
            let cent = project vw res (Coords ptX ptY) in
            zoomTo zoomF cent vw
          renderImage msg img viewVal = do
            element img # set Attr.src ""
            element msg # set text "rendering please wait ..."
            liftIO $ createImageParallel viewVal maxSteps res "./mandelbrot.png"
            element msg # set text ""
            element img # set Attr.src "/static/mandelbrot.png"
            return ()        

mkImage :: String -> UI Element
mkImage source = do
    img <- UI.image #. "mandelImage"
    element img # set Attr.src source