{-# LANGUAGE OverloadedStrings #-}
module C4 where

import qualified SDL
import SDL (quit, ($=))
import qualified SDL.Image as SDL
import Reactive.Banana
import Reactive.Banana.Frameworks
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad
import System.Exit (exitSuccess)
import Control.Monad.Loops (whileJust_)

type P2 = Point V2 CInt

primer :: Point V2 CInt
primer = P (V2 0 0)

rectangle :: CInt -> CInt -> CInt -> CInt -> SDL.Rectangle CInt
rectangle x y w h = SDL.Rectangle (P (V2 x y)) (V2 w h)

fps :: Integer
fps = 60

dt :: Integer
dt = 1000 `div` fps

animation :: CInt -> CInt
animation amoute = if odd n
                      then 500 - x
                      else x
    where (n,x) = amoute `divMod` 500
main :: IO ()
main = do
    (mouseHandler, fireMouse) <- newAddHandler
    (quitHandler, fireQuit) <- newAddHandler
    (tickHandler, fireTick)  <- newAddHandler
    network <- compile $ do
        eMouse <- fromAddHandler mouseHandler
        eQuit <- fromAddHandler quitHandler
        eTick <- fromAddHandler tickHandler
        (window,render, texture) <- liftIO initSDL
        let
            ePos' = filterE (\m -> SDL.mouseButtonEventMotion m == SDL.Pressed) eMouse
            ePos = SDL.mouseButtonEventPos <$> ePos'
            onExit = do
                SDL.destroyRenderer render
                SDL.destroyWindow window
                quit
                exitSuccess
            draw pos t = do
                SDL.clear render
                SDL.copy render texture (Just $ SDL.Rectangle primer (V2 64 205)) (Just $ SDL.Rectangle pos (V2 64 205))
                SDL.rendererDrawColor render $= V4 141 238 238 100
                SDL.copy render texture Nothing (Just $ SDL.Rectangle (P (V2 t 0)) (V2 200 205))
                SDL.present render

            eQuit' :: Event (IO ())
            eQuit' = onExit <$ eQuit
            eAccTick = (+1) <$ eTick

        bDestPos <- stepper primer (fmap fromIntegral <$> ePos)
        bQuit <- stepper (return ()) eQuit'
        bAccTick <- fmap animation <$> accumB 0 eAccTick

        let
            bAll = (>>) <$> (draw <$> bDestPos <*> bAccTick) <*> bQuit
        reactimate $ bAll <@ eTick

    actuate network
    let
        fireInput (SDL.MouseButtonEvent m) = fireMouse m
        fireInput SDL.QuitEvent = fireQuit ()
        fireInput _ = return ()

        processEvents = do
            minput <- SDL.pollEvent
            whileJust_ (SDL.pollEvent) (fireInput . SDL.eventPayload)
    forever $ do
        processEvents
        SDL.delay 16
        fireTick ()

initSDL :: IO (SDL.Window, SDL.Renderer, SDL.Texture)
initSDL = do
    SDL.initializeAll
    window <- SDL.createWindow "Hello" SDL.defaultWindow
    render <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.loadTexture render "./assets/hello_world.bmp"
    return (window, render, texture)

