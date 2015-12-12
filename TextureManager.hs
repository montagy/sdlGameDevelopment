module TextureManager (
    TextureManager
  , addTexture
)where
import Data.Text (Text)
import SDL
import SDL.Image (loadTexture)
import Data.Map.Strict (singleton, Map)

type Key = Text
type TextureManager = Map Key Texture

addTexture :: Key -> FilePath -> Renderer ->  IO TextureManager
addTexture key file render = do
    texture <- loadTexture render file
    return $ singleton key texture
