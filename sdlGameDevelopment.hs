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

spriteSize :: V2 CInt
spriteSize = V2 64 205

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

    (onMouseClick, fireMouseClick) <- B.newAddHandler
    network <- B.compile $ makeNetwork onMouseClick render texture
    B.actuate network
    eventLoop fireMouseClick

    destroyRenderer render
    destroyWindow window
    quit

drawPic :: Renderer -> Texture -> MouseButtonEventData -> IO ()
drawPic render texture mData = do
    clear render
    copy render texture (Just clip1) $
        Just (Rectangle (fromIntegral <$> mouseButtonEventPos mData) spriteSize)
    present render

makeNetwork :: B.AddHandler MouseButtonEventData -> Renderer -> Texture ->  B.MomentIO ()
makeNetwork mClick render texture = do
    eM <- B.fromAddHandler mClick
    B.reactimate $ drawPic render texture <$> eM

eventLoop :: B.Handler MouseButtonEventData -> IO ()
eventLoop fireClick = loop
    where loop = do
            e <- pollEvent
            case eventPayload <$> e of
                Nothing -> loop
                Just e' -> case e' of
                                QuitEvent -> return ()
                                MouseButtonEvent m  -> if mouseButtonEventMotion m== Pressed
                                                                then fireClick m >> loop
                                                                else loop
                                _ -> loop



