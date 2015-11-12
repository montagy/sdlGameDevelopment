{-# LANGUAGE RecordWildCards #-}
module Animation where

import SDL
import Foreign.C.Types

data SpriteSheet = SpriteSheet { fill :: FilePath
                               , width :: CInt
                               , height :: CInt
                               }

getFrames :: (MonadIO m) => SpriteSheet -> Texture -> m Int
getFrames SpriteSheet{..} t = do
    RendererInfo {..} <- getRendererInfo t
    let w = rendererInfoMaxTextureWidth `div` width
        h = rendererInfoMaxTextureHeight `div` height
    return w * h



