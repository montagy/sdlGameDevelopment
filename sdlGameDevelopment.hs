{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import SDL
import qualified SDL.Image as IM
import Control.Concurrent (threadDelay)
import Linear
import Linear.Affine
import Foreign.C.Types
import Control.Monad (when, forever, unless)
import Data.Monoid
import Data.Foldable
import Data.Maybe
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import Data.Word
import Data.IORef
import qualified Data.HashMap.Lazy as HM
import Data.Text (Text, pack)

type Assets = IORef (HM.HashMap Text Texture)
spriteSize :: V2 CInt
spriteSize = V2 64 205
primer :: Point V2 CInt
primer = P (V2 0 0)
clip1, clip2, clip3, clip4 :: Rectangle CInt
clip1 = Rectangle (P (V2 0 0)) spriteSize
clip2 = Rectangle (P (V2 64 0)) spriteSize
clip3 = Rectangle (P (V2 128 0)) spriteSize
clip4 = Rectangle (P (V2 192 0)) spriteSize
fps :: Word32
fps = 60
delayTime :: Word32
delayTime = 1000 `div` 60

data State = State {
        runState :: Renderer -> Assets -> EventSource (Point V2 CInt) ->  IO Bool
    }
main :: IO ()
main = do
    initializeAll
    window <- createWindow "你好" defaultWindow {
            windowPosition = Centered
        }

    render <- createRenderer window (-1 :: CInt) defaultRenderer
    texture1 <- IM.loadTexture render "assets/animation.bmp"
    texture2 <- IM.loadTexture render "assets/hello_world.bmp"
    assets <- newIORef $ HM.singleton (pack "hello") texture2

    currentState <- newIORef $ State gameState

    source <- B.newAddHandler
    network <- B.compile $ makeNetwork source render assets
    B.actuate network

    eventLoop render assets currentState source

    destroyRenderer render
    destroyWindow window
    quit

gameState :: Renderer -> Assets -> EventSource (Point V2 CInt) -> IO Bool
gameState render assets source = do
    texture<- B.liftIO $ (HM.! "hello") <$> readIORef assets
    mevent <- pollEvent
    esc <- case mevent of
                Nothing -> return False
                Just e -> case eventPayload e of
                            QuitEvent -> return True
                            MouseButtonEvent (MouseButtonEventData _ pressed _ _ _ pos)
                                | pressed == Pressed -> fire source (fromIntegral <$> pos) >> return False
                            _ -> return False
    drawOnEveryFps render texture
    present render
    return esc

eventLoop :: Renderer -> Assets -> IORef State -> EventSource (Point V2 CInt)-> IO ()
eventLoop render assets stateRef source = loop
    where loop = do
            frameStart <- ticks
            state <- readIORef stateRef
            esc <- runState state render assets source
            frameTime <- (subtract frameStart) <$> ticks
            if frameTime < delayTime
               then delay (delayTime - frameTime)
               else return ()
            when (not esc) loop

makeNetwork :: EventSource (Point V2 CInt)-> Renderer -> Assets ->  B.MomentIO ()
makeNetwork source render assets = do
    eM <- B.fromAddHandler $ addHandler source
    texture<- B.liftIO $ (HM.! "hello") <$> readIORef assets
    bdestPos <- B.accumB undefined (const <$> eM) -- undefined is ok, need refactor
    edestPos <- B.changes bdestPos

    B.reactimate' $ fmap (drawPic render texture) <$> edestPos

drawPic :: Renderer -> Texture -> Point V2 CInt -> IO ()
drawPic render texture p = do
    clear render
    copy render texture Nothing (Just $ Rectangle p spriteSize)

drawOnEveryFps :: Renderer -> Texture -> IO ()
drawOnEveryFps render texture= do
    copy render texture Nothing (Just $ Rectangle (P (V2 100 100)) spriteSize)

{-----------------------------
 尝试结合sdl和banana
------------------------------}
collectEvents :: IO [Event]
collectEvents = do
    e <- pollEvent
    case e of
      Nothing -> return []
      Just e' -> (e' :) <$> collectEvents

type EventSource a = (B.AddHandler a, B.Handler a)

addHandler :: EventSource a -> B.AddHandler a
addHandler = fst

fire :: EventSource a -> B.Handler a
fire = snd

