{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import SDL
import Control.Concurrent (threadDelay)
import Linear
import Linear.Affine
import Foreign.C.Types

main :: IO ()
main = do
    initializeAll
    window <- createWindow "你好" defaultWindow {
            windowPosition = Centered
        }

    render <- createRenderer window (-1 :: CInt) defaultRenderer

    surface <- loadBMP "assets/animation.bmp"
    texture <- createTextureFromSurface render surface
    freeSurface surface

    --TextureInfo {..} <- queryTexture texture

    let
        spriteSize = V2 64 205
        clip1 = Rectangle (P (V2 0 0)) spriteSize
        clip2 = Rectangle (P (V2 64 0)) spriteSize
        clip3 = Rectangle (P (V2 128 0)) spriteSize
        clip4 = Rectangle (P (V2 192 0)) spriteSize
    rendererDrawColor render $= V4 0 0 0 255
    let
        loop [] = return ()
        loop (frame : frames) = do
            clear render
            copy render texture
                -- source rect
                (Just frame)
                --destination rect
                (Just (Rectangle (P (V2 0 0)) spriteSize))
            present render
            loop frames

    loop $ cycle $  [clip1, clip2, clip3, clip4] >>= replicate 4
    threadDelay 1000000
    destroyRenderer render
    destroyWindow window
    quit

