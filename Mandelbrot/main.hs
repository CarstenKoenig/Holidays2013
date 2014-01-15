module Main where

import Mandelbrot

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, getChanContents, writeChan)
import System.Directory (doesFileExist)

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Attributes as Attr
import qualified Graphics.UI.Threepenny.Events as Ev

import Graphics.UI.Threepenny.Core

data Settings = Settings
    { resolution  :: PictureSize
    , zoom        :: Double
    , renderSteps :: Steps}

data MandelbrotDisplay = MandelbrotDisplay
    { view       :: Behavior ViewWindow
    , visual     :: Element
    }

instance Widget MandelbrotDisplay where
    getElement = visual    


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

setup :: Settings -> Window -> UI ()
setup settings w = void $ do
        return w # set title "Mandelbrot generator "
        md <- mandelbrotDisplay settings
        getBody w #+ [element md]

mandelbrotDisplay :: Settings -> UI MandelbrotDisplay
mandelbrotDisplay settings = do
    img <- mkImage
    msg <- string promptClick
    d   <- UI.div #. "wrap" #+ [element msg, element img]

    -- we want to run the rendering in another thread
    (eDone, run) <- liftIO newAsync

    let
        eMousedown = Ev.mousedown img

        makeViewChange (ptX, ptY) vw = zoomTo (zoom settings) cent vw
            where cent = project vw (resolution settings) (Coords ptX ptY)

        render vw = liftIO . run $ renderView settings vw (localPath renderImageName)

        setImage () = element img # set Attr.src (clientPath renderImageName)

        showPrompt isRendering = do
            element msg # set text (if isRendering then waitMessage else promptClick)


    -- define behaviours (start rendering on mousedown, stop on end of rendering)
    rendering <- stepper False $ unionWith (||) (const False <$> eDone) (const True <$> eMousedown)
    -- change the view based on clicked position if not currently rendering
    vw        <- accumB startView $ makeViewChange <$> whenE (not <$> rendering) eMousedown

    -- hook up side-effects
    onEvent   eDone     setImage
    onChanges vw        render
    onChanges rendering showPrompt

    return $ (MandelbrotDisplay vw d)
    where promptClick = "click anywhere to zoom in to this point"
          waitMessage = "rendering please wait ..."

renderView :: Settings -> ViewWindow -> FilePath -> IO()
renderView settings vw = createImageParallel vw (renderSteps settings) (resolution settings)

mkImage :: UI Element
mkImage = do
    img <- UI.image #. "mandelImage"
    element img # set Attr.src (clientPath startImageName)

-- | Perform an 'IO' operation asynchronously.
-- The 'Event' indicates when the action has finished.
-- (shamelessly copy&pasted from Heinrich Apfelmus)
newAsync :: IO (Event (), Handler (IO ()))
newAsync = do
    (eDone, done) <- newEvent
    
    chan <- newChan 
    forkIO $ do
        ms <- getChanContents chan
        forM_ ms $ \m -> m >> done ()
    
    return (eDone, writeChan chan)


verifyStartImageFile :: Settings -> FilePath -> IO()
verifyStartImageFile settings filePath = do
    startExist <- doesFileExist filePath
    when (not startExist) $ do
        putStrLn "creating starting image - one moment please..."
        renderView settings startView filePath
        putStrLn "done"