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
    image     <- mkImage "/static/mandelbrot.png"
    (_, view) <- mkButton "default View" $ \ () -> do
                    element image # set Attr.src ""
                    element message # set text "rendering please wait ..."
                    liftIO $ render
                    element message # set text ""
                    element image  # set Attr.src "/static/mandelbrot.png"
                    return ()

    getBody w #+
        [UI.div #. "wrap" #+ [element view, element message, element image]]

mkImage :: String -> UI Element
mkImage source = do
    image <- UI.image #. "mandelImage"
    element image # set Attr.src source

mkButton :: String -> (() -> UI ()) -> UI (Element, Element)
mkButton caption onClick = do
    button <- UI.button #. "button" #+ [string caption]
    on UI.click button $ \_ -> onClick()
    view <- UI.p #+ [element button]
    return (button, view)

render :: IO()
render = do
    let view       = View (C (-2.4) 1.2) (C 1.1 (-1.4))
    let maxSteps   = 5000
    let resolution = Size 1024 711
    createImageParallel view maxSteps resolution "./mandelbrot.png"