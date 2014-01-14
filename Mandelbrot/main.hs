module Main where

import Mandelbrot

import Control.Monad
import System.Directory (doesFileExist)

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import qualified Graphics.UI.Threepenny.Events as Ev

import Graphics.UI.Threepenny.Core

data Settings = Settings
    { resolution  :: PictureSize
    , zoom        :: Double
    , renderSteps :: Steps}

defaultSettings :: Settings
defaultSettings = Settings defaultRes defaultZoom defaultSteps
    where
    defaultRes     = Size 1024 711
    defaultSteps   = 1024
    defaultZoom    = 4

startView :: ViewWindow
startView          = View (C (-2.4) 1.2) (C 1.1 (-1.4))

startImageName :: String
startImageName = "start.png"

renderImageName :: String
renderImageName = "mandelbrot.png"

localPath :: String -> FilePath
localPath name = "./" ++ name

clientPath :: String -> String
clientPath name = "/static/" ++ name

main :: IO ()
main = do

    let settings = defaultSettings

    -- create the start-image if neccessary
    verifyStartImageFile settings $ localPath startImageName

    putStrLn "starting local server..."
    startGUI defaultConfig
        { tpPort = 10000
        , tpStatic = Just "./"
        } (setup settings)

setup :: Settings -> Window -> UI ()
setup settings w = void $ do
        return w # set title "Mandelbrot generator "
        -- UI.addStyleSheet w "buttons.css"

        md <- mandelbrotDisplay settings
        getBody w #+ [element md]

data MandelbrotDisplay = MandelbrotDisplay
    { view       :: Behavior ViewWindow
    , visual     :: Element
    }

instance Widget MandelbrotDisplay where
    getElement = visual    

mandelbrotDisplay :: Settings -> UI MandelbrotDisplay
mandelbrotDisplay settings = do
    img <- mkImage
    msg <- string "click to zoom in"
    d   <- UI.div #. "wrap" #+ [element msg, element img]

    (evRendering, setRendering) <- liftIO $ newEvent
    rendering <- stepper False evRendering

    let
        onRendering r = if r then const Nothing else Just . id

        filteredMouseDown = filterJust . apply (onRendering <$> rendering) $ Ev.mousedown img

        makeViewChange (ptX, ptY) vw = zoomTo (zoom settings) cent vw
            where cent = project vw (resolution settings) (Coords ptX ptY)

        renderImage viewVal = void $ do
            liftIO $ setRendering True
            liftIO $ renderView settings viewVal (localPath renderImageName)
            element img # set Attr.src (clientPath renderImageName)
            liftIO $ setRendering False

    vw <- accumB startView $ makeViewChange <$> filteredMouseDown
    onChanges vw renderImage
    onChanges rendering (\r -> do element msg # set text (if r then "rendering please wait ..." else "click to zoom in"))

    return $ (MandelbrotDisplay vw d)

mkImage :: UI Element
mkImage = do
    img <- UI.image #. "mandelImage"
    element img # set Attr.src (clientPath startImageName)

verifyStartImageFile :: Settings -> FilePath -> IO()
verifyStartImageFile settings filePath = do
    startExist <- doesFileExist filePath
    when (not startExist) $ do
        putStrLn "creating starting image - one moment please..."
        renderView settings startView filePath
        putStrLn "done"

renderView :: Settings -> ViewWindow -> FilePath -> IO()
renderView settings vw = createImageParallel vw (renderSteps settings) (resolution settings)

