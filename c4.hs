{-# LANGUAGE OverloadedStrings #-}
module C4 where

import qualified SDL
import SDL (quit)
import qualified SDL.Image as SDL
import Reactive.Banana
import Reactive.Banana.Frameworks
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad

primer :: Point V2 CInt
primer = P (V2 0 0)

main :: IO ()
main = do
    (mouseHandler, fireMouse) <- newAddHandler
    (quitHandler, fireQuit) <- newAddHandler
    network <- compile $ do
        eMouse <- fromAddHandler mouseHandler
        eQuit <- fromAddHandler quitHandler
        (window,render, texture) <- liftIO initSDL
        let
            ePos' = filterE (\m -> SDL.mouseButtonEventMotion m == SDL.Pressed) eMouse
            ePos = (\m -> SDL.mouseButtonEventPos m ) <$> ePos'
            eQuit :: Event (IO ())
            eQuit = (\_ -> do
                    SDL.destroyRenderer render
                    SDL.destroyWindow window
                    quit) <$> eQuit
        bDestPos <- stepper primer (fmap fromIntegral <$> ePos)
        --eDestPos <- changes bDestPos
        reactimate $ (\pos -> do
            SDL.clear render
            SDL.copy render texture (Just $ SDL.Rectangle primer (V2 64 205)) (Just $ SDL.Rectangle pos (V2 64 205))
            SDL.present render) <$> bDestPos <@ eQuit

    actuate network
    let
        fireInput Nothing = return ()
        fireInput (Just (SDL.MouseButtonEvent m)) = fireMouse m
        fireInput (Just SDL.QuitEvent) = fireQuit ()
        fireInput _ = return ()

        processEvents = do
            minput <- SDL.pollEvent
            unless (minput == Nothing) $ do
                fireInput (SDL.eventPayload <$> minput)
                processEvents
        go = do
            processEvents
            go
    go
type P2 = Point V2 CInt

initSDL :: IO (SDL.Window, SDL.Renderer, SDL.Texture)
initSDL = do
    SDL.initializeAll
    window <- SDL.createWindow "Hello" SDL.defaultWindow
    render <- SDL.createRenderer window (-1) SDL.defaultRenderer
    texture <- SDL.loadTexture render "./assets/hello_world.bmp"
    return (window, render, texture)


drawPure :: SDL.Renderer -> P2 -> IO ()
drawPure render pos = do
    texture <- SDL.loadTexture render "./assets/hello_world.bmp"
    SDL.copy render texture (Just $ SDL.Rectangle (P (V2 0 0)) (V2 64 205)) (Just $ SDL.Rectangle pos (V2 64 205))
