{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy               as B
import qualified Data.ByteString.Lazy.Char8         as B8
import           Data.Maybe
import           Data.Text.Lazy                     (Text)
import qualified Data.Text.Lazy                     as T
import qualified Data.Traversable                   as Tr
import           Data.UUID                          (UUID)
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V5                       as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
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

initTable :: Query
initTable = "create table if not exists paste (\
\   paste_id uuid primary key,\
\   lang Text not null,\
\   contents Text not null);"

data Paste = Paste { pasteId      :: UUID
                   , pasteLang    :: String
                   , pasteContent :: B.ByteString
                   } deriving (Show)

instance FromRow Paste where
  fromRow = Paste <$> field <*> field <*> field

nsMinipaste :: UUID
nsMinipaste = UUID.generateNamed UUID.namespaceURL (B.unpack "github.com/jtanguy/minipaste")

getPaste :: Connection -> UUID -> IO (Maybe Paste)
getPaste conn uid = listToMaybe <$> query conn q (Only uid)
  where q = "select * from paste where paste_id = ?"

postPaste :: Connection -> Paste -> IO ()
postPaste conn (Paste u l c) = execute conn q (u,l,c) >> return ()
  where q = "insert into paste (paste_id,lang,contents) values (?,?,?)"

patchPaste :: Connection -> UUID -> String -> IO ()
patchPaste conn uid lang = execute conn q (uid, lang) >> return ()
  where q = "update paste set lang = ? where paste_id = ?"

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
           $ formatHtmlBlock defaultFormatOpts{numberLines=True}
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
            _ <- execute_ conn initTable
            scotty 8080 $ do
                get "/:uid" $ do
                    u <- param "uid"
                    p <- Tr.mapM (liftIO . getPaste conn) (UUID.fromString u)
                    case join p of
                        Just paste -> S.html $ formatPaste paste
                        Nothing -> status status404 >> S.html "<h1>Paste not found</h1>"
                patch "/:uid/:lang" $ do
                    u <- param "uid"
                    l <- param "lang"
                    _<- Tr.mapM (liftIO . flip (patchPaste conn) l) (UUID.fromString u)
                    redirect $ T.pack ('/':u)
                post "/:lang" $ do
                    l <- param "lang"
                    c <- S.body
                    let uid = UUID.generateNamed nsMinipaste (B.unpack c)
                    liftIO $ postPaste conn (Paste uid l c)
                    redirect $ T.pack ('/': (UUID.toString uid))


