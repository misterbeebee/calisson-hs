{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- Image-serving framework copied from
-- https://www.fpcomplete.com/user/edwardk/cellular-automata/part-1

import           Config
import qualified Data.ByteString              as Strict
import qualified Data.ByteString.Lazy         as Lazy
import           Data.Text
import qualified Debug.Trace                  as DTrace
import           Diagram                      (diagram)
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Hexagrid.Grid                (Spec, mkSpec)
import           Text.Blaze.Svg.Renderer.Utf8
import           Yesod

main = DTrace.trace "main" $ 
    warpEnv App   -- warpEnv uses default middleware, including gzip compression

data App = App
instance Yesod App where
    shouldLog App src level =
        True -- good for development
        -- level == LevelWarn || level == LevelError -- good for production

-- boilerplate to make getImageR's type signature support FormMessage
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|
  / ImageR GET
|]

svg :: Diagram SVG R2 -> Strict.ByteString
svg = Strict.concat . Lazy.toChunks . renderSvg . renderDia SVG (SVGOptions (Width 600) Nothing)

getImageR :: ((RenderMessage (HandlerSite m) FormMessage), MonadHandler m) => m TypedContent
getImageR =
    DTrace.trace "getImageR" $
    do
      input <- runInputGet $ Input
               <$> iopt intField (pack "radius")
               <*> iopt intField (pack "shuffles")
      let spec = mkSpec (safeRadius input) (safeShuffles input) thePositionEntropy
      return $ toTypedContent (typeSvg, toContent . svg $ dia spec)

dia :: Spec source -> Diagram SVG R2
dia = diagram
