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
import Data.Text (Text)

data State = State {
      preload :: IO ()
    , update :: IO ()
    }
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
main :: IO ()
main = do
    initializeAll
    window <- createWindow "你好" defaultWindow {
            windowPosition = Centered
        }

    render <- createRenderer window (-1 :: CInt) defaultRenderer
    texture1 <- IM.loadTexture render "assets/animation.bmp"
    texture2 <- IM.loadTexture render "assets/hello_world.bmp"
    assets <- newIORef $ (HM.singleton "animation" texture1 <> HM.singleton "hello" texture2)
    currentState <- newIORef "Game"

    state <- readIORef currentState
    case state of
      "Game" -> gameState render assets
      "Menu" -> menuState render assets
      _ -> return ()

    destroyRenderer render
    destroyWindow window
    quit

gameState :: Renderer -> Assets -> IO ()
gameState render assets = do
    clear render
    drawOnEveryFps render assets
    present render
    source <- B.newAddHandler
    network <- B.compile $ makeNetwork source render assets
    B.actuate network
    eventLoop source

menuState :: Renderer -> Assets -> IO ()
menuState render assets = do
    texture <- (HM.! "hello") <$> readIORef assets
    clear render
    copy render texture Nothing Nothing
    present render

drawPic :: Renderer -> Assets -> Point V2 CInt -> IO ()
drawPic render assets p = do
    clear render
    drawOnEveryFps render assets
    texture<- (HM.! "animation") <$> readIORef assets
    copy render texture Nothing (Just $ Rectangle p spriteSize)
    present render

drawOnEveryFps :: Renderer -> Assets -> IO ()
drawOnEveryFps render assets = do
    texture <- (HM.! "hello") <$> readIORef assets
    copy render texture Nothing (Just $ Rectangle (P (V2 100 100)) spriteSize)

makeNetwork :: EventSource (Point V2 CInt)-> Renderer -> Assets ->  B.MomentIO ()
makeNetwork source render assets = do
    eM <- B.fromAddHandler $ addHandler source
    bdestPos <- B.accumB undefined (const <$> eM) -- undefined is ok, need refactor
    edestPos <- B.changes bdestPos

    B.reactimate' $ fmap (drawPic render assets) <$> edestPos

eventLoop :: EventSource (Point V2 CInt) -> IO ()
eventLoop source = loop
    where loop = do
            mevent <- pollEvent
            quit <- case mevent of
                        Nothing -> return True
                        Just e -> case eventPayload e of
                                    QuitEvent -> return False
                                    MouseButtonEvent (MouseButtonEventData _ pressed _ _ _ pos)
                                        | pressed == Pressed -> fire source (fromIntegral <$> pos) >> return True
                                    _ -> return True
            delay 10
            when quit loop

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

