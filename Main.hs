{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- Image-serving framework copied from
-- https://www.fpcomplete.com/user/edwardk/cellular-automata/part-1

import           Config              (Input (Input), inputRadius, inputShuffles,
                                      safeRadius, safeShuffles,
                                      thePositionEntropy)
import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe          (fromMaybe)
import           Data.Text           (pack)
import qualified Debug.Trace         as DTrace
import           Diagram             (dia, svg)
import           Hexagrid.Grid       (Spec, mkSpec)
import           Text.Blaze          (ToMarkup)
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
  /img ImageR GET
  / PageR GET
|]

data PageInput = PageInput {
    pageInputRadius   :: Maybe Int,
    pageInputShuffles :: Maybe Int
}

defaultRadius = 6

defaultShuffles radius =
    -- number of movable tiles ~ r*2, and distance they can fall is r, so: ~r^3
    radius * radius * radius `div` 2

inputForm :: ((RenderMessage (HandlerSite m) FormMessage), Monad m) => FormInput m PageInput
inputForm = PageInput
  <$> iopt intField (pack "radius")
  <*> iopt intField (pack "shuffles")

runParseForm :: FormInput (HandlerT App IO) PageInput -> HandlerT App IO Input
runParseForm inputForm = do
  formInput <- runInputGet inputForm
  let get def field = fromMaybe def (field formInput) 
  let radius = (safeRadius $ get defaultRadius pageInputRadius)
  return $ Input radius
    (safeShuffles $ get (defaultShuffles radius) pageInputShuffles)

getImageR :: HandlerT App IO TypedContent
getImageR =
    DTrace.trace "getImageR" $
    do
      -- input <- runInputGet inputAForm defaultInput
      (Input radius shuffles) <- runParseForm inputForm
      let spec = mkSpec radius shuffles thePositionEntropy
      return $ toTypedContent (typeSvg, toContent . svg $ dia spec)

getPageR :: Handler Html
getPageR = do
    -- let Input radius shuffles = defaultInput
    (Input radius shuffles) <- runParseForm inputForm
    defaultLayout $ do
      toWidget [lucius|
        input.int-input {
          border-style: dotted;
          border-width: 1px;
          text-align: center;
          font-size: 12pt;
        }
      |]
      [whamlet|$newline never
        <div style=float:left>
          <form method=get action=@{PageR}>
             Size: <input class="int-input" type=text size=1 name=radius onchange=this.form.submit() value=#{radius}><br/>
             Shuffles: <input class="int-input" type=text size=4 name=shuffles onchange=this.form.submit() value=#{shuffles}><br/>
             <input .btn type=submit value="submit">
        <br>
        <div style=float:left>
           <img src=/img?radius=#{radius}&shuffles=#{shuffles}>

      |]
