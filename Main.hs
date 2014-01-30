{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Comonad
import Control.Lens as L
import Data.Bits
import Data.Bits.Lens as L
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.MemoCombinators
import Data.Word
import Diagrams.Backend.SVG
import Diagrams.Prelude as D
import Text.Blaze.Svg.Renderer.Utf8
import Yesod
import Diagram(diagram)

data Store s a = Store (s -> a) s deriving Functor

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
  
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s 

rule :: Num s => Word8 -> Store s Bool -> Bool
rule w (Store f s) = testBit w $ 0 L.& partsOf (taking 3 L.bits) .~ [f (s+1), f s, f (s-1)]

tab :: Memo s -> Store s a -> Store s a
tab opt (Store f s) = Store (opt f) s

loop :: Integral s => (Store s a -> a) -> Store s a -> [Store s a]
loop f = iterate (extend f . tab integral)

window :: (Enum s, Num s) => s -> s -> Store s a -> [a]
window l h = experiment $ \ s -> [s-l..s+h]

grid :: [[Bool]] -> Diagram SVG R2
grid = cat unitY . reverse . map (hcat . map cell) where
  cell b = unitSquare D.# fc (if b then black else white)

svg :: Diagram SVG R2 -> Strict.ByteString
svg = Strict.concat . Lazy.toChunks . renderSvg . renderDia SVG (SVGOptions (Width 600) Nothing)
 
data App = App

instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typeSvg, toContent img) 

-- img = lifeImg
img = svg dia

dia :: Diagram SVG R2 
dia = Diagram.diagram 

life :: Diagram SVG R2 
life =  grid . map (window 49 0) . take 50 . loop (rule 110) $ Store (==0) (0 :: Int)

main = warpEnv App
