{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import qualified SDL
import SDL (quit, ($=))
import SDL.Image ()
import Reactive.Banana
import Reactive.Banana.Frameworks
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad
import System.Exit (exitSuccess)
import Control.Monad.Loops (whileJust_)
import Data.Monoid ((<>))

import qualified Data.Map.Strict as Map

import TextureManager

type P2 = Point V2 CInt

primer :: P2
primer = P (V2 0 0)

rectangle :: CInt -> CInt -> CInt -> CInt -> SDL.Rectangle CInt
rectangle x y w h = SDL.Rectangle (P (V2 x y)) (V2 w h)


fps, dt, speed, frames :: Integral a => a
fps = 60
dt = 1000 `div` fps
speed = 10
frames = 4

animation :: Integral a => a -> a
animation amoute = (amoute `div` speed) `mod` frames

data Ctx = Ctx { window :: SDL.Window
               , render :: SDL.Renderer
               , textureManager :: TextureManager
               }
main :: IO ()
main = do
    (mouseHandler, fireMouse) <- newAddHandler
    (quitHandler, fireQuit) <- newAddHandler
    (tickHandler, fireTick)  <- newAddHandler
    let
        fireInput (SDL.MouseButtonEvent m) = fireMouse m
        fireInput SDL.QuitEvent = fireQuit ()
        fireInput _ = return ()

        processEvents = whileJust_ SDL.pollEvent (fireInput . SDL.eventPayload)
    network <- compile $ do
        eMouse <- fromAddHandler mouseHandler
        eQuit <- fromAddHandler quitHandler
        eTick <- fromAddHandler tickHandler
        ctx@Ctx{..} <- liftIO initSDL
        let
            onExit = do
                SDL.destroyRenderer render
                SDL.destroyWindow window
                quit
                exitSuccess
            eQuit' = onExit <$ eQuit
            eAccTick = (+1) <$ eTick

        bDestPos <- stepper primer (eMouseClickPosition eMouse)
        bQuit <- stepper (return ()) eQuit'
        bAccTick <- fmap animation <$> accumB 0 eAccTick
        let
            bAll = (>>) <$> (draw ctx <$> bDestPos <*> bAccTick) <*> bQuit
        reactimate $ bAll <@ eTick

    actuate network
    forever $ do
        fireTick ()
        processEvents
        SDL.delay dt

initSDL :: IO Ctx
initSDL = do
    SDL.initializeAll
    window <- SDL.createWindow "Hello" SDL.defaultWindow
    render <- SDL.createRenderer window (-1) SDL.defaultRenderer
    textureManager <- (<>) <$>
        addTexture "walker" "assets/animation.bmp" render <*>
            addTexture "button" "assets/button.bmp" render
    return Ctx{..}

draw :: Ctx -> P2 -> CInt -> IO ()
draw Ctx{..} pos t = do
    SDL.clear render
    SDL.rendererDrawColor render $= V4 141 238 238 100
    case Map.lookup "walker" textureManager of
      Nothing -> return ()
      Just walker ->
          SDL.copy render walker (Just $ SDL.Rectangle (P (V2 (64*t) 0)) (V2 64 205)) (Just $ SDL.Rectangle pos (V2 64 205))
    case Map.lookup "button" textureManager of
      Nothing -> return ()
      Just button -> do
          info <- SDL.queryTexture button
          SDL.copy render button Nothing (Just $ SDL.Rectangle (P (V2 100 100)) (V2 (SDL.textureWidth info) (SDL.textureHeight info)))
    SDL.present render

eMouseClickPosition :: Event SDL.MouseButtonEventData -> Event P2
eMouseClickPosition  e =
  fmap fromIntegral . SDL.mouseButtonEventPos <$>
    filterE (\m -> SDL.mouseButtonEventMotion m == SDL.Pressed) e
