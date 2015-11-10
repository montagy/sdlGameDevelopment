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

    drawPic render texture (Just primer)
    eventLoop source

    destroyRenderer render
    destroyWindow window
    quit

drawPic :: Renderer -> Texture -> Maybe (Point V2 CInt) -> IO ()
drawPic render texture mData = do
    clear render
    copy render texture (Just clip1) (Just $ Rectangle destPoint spriteSize)
    present render
        where
            destPoint = case mData of
                          Nothing -> primer
                          Just p -> p

makeNetwork :: EventSource Event -> Renderer -> Texture ->  B.MomentIO ()
makeNetwork source render texture = do
    eM <- B.fromAddHandler $ addHandler source
    let eMouseClick = B.filterE getMouseClick eM
    B.reactimate $ drawPic render texture <$> getMouseClickPos <$> eMouseClick

getMouseClickPos :: Event -> Maybe (Point V2 CInt)
getMouseClickPos e = case eventPayload e of
                       MouseButtonEvent m -> Just $ fromIntegral <$> mouseButtonEventPos m
                       _ -> Nothing
getMouseClick :: Event -> Bool
getMouseClick e =
    case eventPayload e of
        MouseButtonEvent m -> if mouseButtonEventMotion m == Pressed
                                then True
                                else False
        _ -> False

eventLoop :: EventSource Event -> IO ()
eventLoop source = loop
    where loop = do
            e <- collectEvent
            case eventPayload e of
              QuitEvent -> return ()
              _ -> fire source e >> loop

{-----------------------------
 尝试结合sdl和banana
------------------------------}
collectEvent :: IO Event
collectEvent = do
    e <- pollEvent
    case e of
      Nothing -> collectEvent
      Just e' -> return e'

type EventSource a = (B.AddHandler a, B.Handler a)
type SDLEventSource = EventSource Event
type SDLEvent = B.Event Event

addHandler :: EventSource a -> B.AddHandler a
addHandler = fst

fire :: EventSource a -> B.Handler a
fire = snd

