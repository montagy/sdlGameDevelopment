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

    surface <- loadBMP "assets/foo.bmp"
    texture <- createTextureFromSurface render surface
    freeSurface surface

    TextureInfo {..} <- queryTexture texture

    rendererDrawColor render $= V4 0 0 0 255
    clear render
    copy render texture
        -- source rect
        (Just (Rectangle (P (V2 0 0)) (V2 textureWidth textureHeight)))
        --destination rect
        (Just (Rectangle (P (V2 0 0)) (V2 (textureWidth*(3::CInt)) (textureHeight*(3::CInt)))))
    present render

    threadDelay 1000000
    quit

