{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-} 
{-# LANGUAGE TemplateHaskell #-}

-- Image-serving framework copied from 
-- https://www.fpcomplete.com/user/edwardk/cellular-automata/part-1

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Text.Blaze.Svg.Renderer.Utf8
import Yesod
import Diagram(diagram)

main = warpEnv App

data App = App
instance Yesod App
mkYesod "App" [parseRoutes| / ImageR GET |]

svg :: Diagram SVG R2 -> Strict.ByteString
svg = Strict.concat . Lazy.toChunks . renderSvg . renderDia SVG (SVGOptions (Width 600) Nothing)
 
getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typeSvg, toContent . svg $ dia) 

dia :: Diagram SVG R2 
dia = diagram 
