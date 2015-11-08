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
    {-texture <- createTextureFromSurface render surface-}
    {-freeSurface surface-}

    let
        loop dest = do
            let collectEvents = do
                    e <- pollEvent
                    case e of
                      Nothing -> return []
                      Just e' -> (e' :) <$> collectEvents

            events <- map eventPayload <$> collectEvents
            let quit = any (== QuitEvent) events
                destPoint = fromMaybe dest $ getLast $
                    foldMap (\case
                                    MouseButtonEvent e -> if mouseButtonEventButton e == ButtonLeft
                                                            then Last (Just $ mouseButtonEventPos e)
                                                            else mempty
                                    _ -> mempty
                            ) events

            clear render
            copy render texture (Just clip1) $
                Just (Rectangle (fromIntegral <$> destPoint) spriteSize)
            present render
            unless quit $ loop destPoint

    loop $ P (V2 0 0)
    threadDelay 3000000
    destroyRenderer render
    destroyWindow window
    quit

