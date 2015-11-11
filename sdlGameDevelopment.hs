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
    texture <- IM.loadTexture render "assets/animation.bmp"

    source <- B.newAddHandler
    network <- B.compile $ makeNetwork source render texture
    B.actuate network

    drawPic render texture primer
    eventLoop source

    destroyRenderer render
    destroyWindow window
    quit

drawPic :: Renderer -> Texture -> Point V2 CInt -> IO ()
drawPic render texture p = do
    clear render
    copy render texture (Just clip1) (Just $ Rectangle p spriteSize)
    present render

makeNetwork :: EventSource (Point V2 CInt)-> Renderer -> Texture ->  B.MomentIO ()
makeNetwork source render texture = do
    eM <- B.fromAddHandler $ addHandler source
    bdestPos <- B.accumB primer (const <$> eM)
    edestPos <- B.changes bdestPos

    B.reactimate' $ fmap (drawPic render texture) <$> edestPos

eventLoop :: EventSource (Point V2 CInt) -> IO ()
eventLoop source = loop
    where loop = do
            events <- collectEvents
            let
                es = eventPayload <$> events
                quit = any (== QuitEvent) es
                pos = getLast $ foldMap (\case
                                                MouseButtonEvent (MouseButtonEventData _ pressed _ _ _ pos)
                                                            | pressed == Pressed -> Last (Just pos)
                                                _ -> Last Nothing
                                        ) es
            case pos of
              Nothing -> return ()
              Just p -> fire source $ fromIntegral <$> p
            threadDelay 10000
            unless quit loop

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
type SDLEventSource = EventSource Event
type SDLEvent = B.Event Event

addHandler :: EventSource a -> B.AddHandler a
addHandler = fst

fire :: EventSource a -> B.Handler a
fire = snd

