{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy               as B
import qualified Data.ByteString.Lazy.Char8         as B8
import           Data.Maybe
import           Data.Text.Lazy                     (Text)
import qualified Data.Traversable                   as T
import           Data.UUID                          (UUID)
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V5                       as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Network.HTTP.Types.Status
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Blaze.Html                    (toHtml)
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import           Text.Blaze.Html5                   as H hiding (param)
import           Text.Blaze.Html5.Attributes        as A
import           Text.Highlighting.Kate
import           Web.Scotty
import qualified Web.Scotty                         as S

data Paste = Paste { pasteId      :: UUID
                   , pasteLang    :: String
                   , pasteContent :: B.ByteString
                   } deriving (Show)

instance FromRow Paste where
  fromRow = Paste <$> field <*> field <*> field

instance ToRow Paste where
  toRow p = [ toField (pasteId p)
            , toField (pasteLang p)
            , toField (pasteContent p)
            ]

nsMinipaste :: UUID
nsMinipaste = UUID.generateNamed UUID.namespaceURL (B.unpack "hackage.haskell.org/package/minipaste")

getPaste :: Connection -> UUID -> IO (Maybe Paste)
getPaste conn uid = listToMaybe <$> query conn "select * from paste where pasteId = ?" (Only uid)

postPaste :: Connection -> Paste -> IO ()
postPaste conn p = (execute conn "insert into paste (?,?,?)" $ p )>> return ()

getConnInfo :: IO (Maybe ConnectInfo)
getConnInfo = do
    host <- lookupEnv "POSTGRESQL_ADDON_HOST"
    port <- lookupEnv "POSTGRESQL_ADDON_PORT"
    user <- lookupEnv "POSTGRESQL_ADDON_USER"
    pwd  <- lookupEnv "POSTGRESQL_ADDON_PASSWORD"
    db   <- lookupEnv "POSTGRESQL_ADDON_DB"
    return $ liftM5 ConnectInfo  host (read <$> port) user pwd db

formatPaste :: Paste -> Text
formatPaste (Paste _ lang code) = renderHtml $ do
    H.head $ H.style ! A.type_ (toValue ("text/css" :: String))
           $ toHtml $ styleToCss zenburn
    H.body $ toHtml
           $ formatHtmlBlock defaultFormatOpts
           $ highlightAs lang (B8.unpack code)

main :: IO ()
main = do
    info <- getConnInfo
    case info of
        Nothing -> do
            hPutStrLn stderr "Could not get connection info from environment"
            exitFailure
        Just i -> do
            conn <- connect i
            scotty 8080 $ do
                get "/:uid" $ do
                    u <- param "uid"
                    p <- T.mapM (liftIO . getPaste conn) (UUID.fromString u)
                    case join p of
                        Just paste -> S.html $ formatPaste paste
                        Nothing -> status status404 >> S.html "<h1>Not found</h1>"
                forM_ languages $ \lang -> do
                    post "/lang/" $ do
                        c <- S.body
                        let uid = UUID.generateNamed nsMinipaste (B.unpack c)
                        liftIO $ postPaste conn (Paste uid lang c)


