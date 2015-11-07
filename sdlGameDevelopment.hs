{-# LANGUAGE OverloadedStrings #-}

import SDL
import Control.Concurrent (threadDelay)
import Linear
import Foreign.C.Types

main :: IO ()
main = do
    initializeAll
    window <- createWindow "你好" defaultWindow {
            windowPosition = Centered
        }
    showWindow window
    render <- createRenderer window (-1 :: CInt) defaultRenderer
    rendererDrawColor render $= V4 0 0 0 255
    clear render
    present render

    threadDelay 1000000
    quit

