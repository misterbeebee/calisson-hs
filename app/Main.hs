{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Config                       (safeRadius, safeShuffles,
                                               thePositionEntropy)
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad                (liftM)
import           Control.Monad.Logger         (runNoLoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (pack)
import           Data.Text                    (Text, pack, unpack)
import           Database.Persist.Class
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Debug.Trace                  as DTrace
import           Diagram                      (dia, svg)
import           Hexagrid.Grid                (Spec, mkSpec)
import           Hexagrid.Tiling              (Tiling, tilingRadius)
import           Text.Blaze                   (ToMarkup)
import           Yesod

-- http://www.yesodweb.com/book/persistent

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBTiling
  value Text
  deriving Show
|]

-- We keep our connection pool in the foundation. At program initialization, we
-- create our initial pool, and each time we need to perform an action we check
-- out a single connection from the pool.

data App = App SqlBackend

main :: IO ()
main = DTrace.trace "main" $
  runStderrLoggingT $ withSqliteConn ":memory:" $ \sqlbackend -> do
     runSqlConn (runMigration migrateAll) sqlbackend
     liftIO $ warp 3000 (App sqlbackend) -- warpEnv uses default middleware, including gzip compression

instance Yesod App where
    shouldLogIO (App pool) src level =
        return True -- good for development
        -- level == LevelWarn || level == LevelError -- good for production

-- boilerplate to make getImageR's type signature support FormMessage
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Now we need to define a YesodPersist instance, which will keep track of
-- which backend we're using and how to run an action.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend 

    runDB action = do
      App sqlBackend <- getYesod
      runSqlConn action sqlBackend


mkYesod "App" [parseRoutes|
  / PageR GET
  /tiling/#DBTilingId TilingR GET

|]

data Input = Input {
    inputRadius   :: Int,
    inputShuffles :: Int,
    inputContinue :: Maybe DBTilingId -- whether to continue shuffling previous diagram
}

data PageInput = PageInput {
    pageInputRadius   :: Maybe Int,
    pageInputShuffles :: Maybe Int,
    pageInputContinue :: Maybe Int -- reuse previous
}

defaultRadius = 6

defaultShuffles radius =
    -- number of movable tiles ~ r*2, and distance they can fall is r, so: ~r^3
    radius * radius * radius `div` 2

inputForm :: ((RenderMessage (HandlerSite m) FormMessage), Monad m) => FormInput m PageInput
inputForm = PageInput
  <$> iopt intField (pack "radius")
  <*> iopt intField (pack "shuffles")
  <*> iopt intField (pack "continue")

-- We'll just return the show value of a tiling, or a 404
-- exist.
getTilingR :: DBTilingId -> HandlerT App IO TypedContent
getTilingR continue = do
    mdbTiling <- runDB $ get continue
    let
      mInputTiling = case mdbTiling  of
        Nothing -> Nothing
        Just (DBTiling tiling) -> Just . (read . unpack)  $ tiling
    case mInputTiling of
        Nothing -> notFound
        -- fixme : extra database read just to get radius
        Just inputTiling ->  doImage (Input (tilingRadius inputTiling) 0 (Just continue))



runParseForm inputForm = do
  formInput <- runInputGet inputForm
  let get def field = fromMaybe def (field formInput)
  let radius = (safeRadius $ get defaultRadius pageInputRadius)
  return $ Input radius
    (safeShuffles $ get (defaultShuffles radius) pageInputShuffles)
    (fmap (DBTilingKey . fromIntegral) (pageInputContinue formInput))

getImageR :: HandlerT App IO TypedContent
getImageR =
    DTrace.trace "getImageR" $
    do
      runParseForm inputForm >>= doImage

doImage :: Input -> HandlerT App IO TypedContent
doImage = liftM snd  . makeImage


makeImage (Input radius shuffles continue) =
    DTrace.trace "getImageR" $
    do
      -- input <- runInputGet inputAForm defaultInput
      let spec = mkSpec radius shuffles thePositionEntropy
      mdbTiling <-
          case continue of
            Nothing -> return Nothing
            Just c -> runDB . get $ c
      let
        mInputTiling = case mdbTiling  of
          Nothing -> Nothing
          Just (DBTiling tiling) -> Just . (read . unpack)  $ tiling
      let continueRadius =  maybe radius tilingRadius mInputTiling
      let (outTiling, diagram) =  dia spec mInputTiling
      let response = toTypedContent (typeSvg, toContent . svg $ diagram)
      tilingId <- runDB $ insert (DBTiling . pack . show $ outTiling)
      return (tilingId, response)

getPageR :: Handler Html
getPageR = do
    -- let (Input radius shuffles continue) = defaultInput
    (Input radius shuffles continue) <- runParseForm inputForm
    let asInt (DBTilingKey x) = toInteger x
    tilingId <- liftM fst $ makeImage (Input radius shuffles continue)
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
             From: #<input class="int-input" type=text size=1 name=continue onchange=this.form.submit() value=#{asInt tilingId}><br/>
             <input .btn type=submit value="submit">
        <br>
        <div style=float:left>
           <img src=/tiling/#{asInt tilingId}>

      |]
