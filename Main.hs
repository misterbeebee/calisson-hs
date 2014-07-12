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
import Data.Text
import Yesod
import Diagram(diagram)
import qualified Debug.Trace as DTrace

main = DTrace.trace "main!" $ warpEnv App

data App = App
instance Yesod App where
    shouldLog App src level =
        True -- good for development
        -- level == LevelWarn || level == LevelError -- good for production
        
mkYesod "App" [parseRoutes|
  / ImageR GET
|]

svg :: Diagram SVG R2 -> Strict.ByteString
svg = Strict.concat . Lazy.toChunks . renderSvg . renderDia SVG (SVGOptions (Width 600) Nothing)

getImageR :: MonadHandler m => m TypedContent
getImageR =
    DTrace.trace "getImageR" $
    sendResponse $ toTypedContent (typeSvg, toContent . svg $ dia) 

dia :: Diagram SVG R2 
dia = diagram 
