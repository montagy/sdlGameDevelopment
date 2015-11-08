{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import SDL
import qualified SDL.Image as IM
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

    surface <- IM.load "assets/spritelib_gpl/fishdish/fishdish_mockup.png"
    texture <- createTextureFromSurface render surface
    freeSurface surface

    clear render
    copy render texture Nothing Nothing
    present render

    threadDelay 3000000
    destroyRenderer render
    destroyWindow window
    quit

