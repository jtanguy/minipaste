{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Minimal pastebin service, powered by Scotty, Blaze, and Hasql
-}
module Main where

import           Control.Applicative
import Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.Char8      as B8
import           Data.Monoid
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.Encoding   as TE
import qualified Data.Traversable          as Tr
import qualified Data.UUID                 as UUID
import qualified Data.UUID.V5              as UUID
import qualified Hasql                     as H
import qualified Hasql.Postgres                     as H
import           Network.HTTP.Types.Status
import Network.Wai
import           Web.Scotty.Trans

import           Database
import           Paste

nsMinipaste :: UUID.UUID
nsMinipaste = UUID.generateNamed UUID.namespaceURL (B.unpack "github.com/jtanguy/minipaste")

data Error = NotFound | Other T.Text

instance ScottyError Error where
    stringError = Other . T.pack
    showError (NotFound) = "Paste not found"
    showError (Other s) = "Unexpected error: " <> s

handler :: Monad m => Error -> ActionT Error m ()
handler NotFound = status status404 >> html "<h1>Paste not found</h1>"
handler (Other s) = do
    status status500
    html $  "<h1>" <> "Unexpected error: " <> s <> "</h1>"

hasqlHandler :: Either (H.SessionError H.Postgres) Response -> Response
hasqlHandler (Right r) = r
hasqlHandler (Left err) = responseLBS status500 [] (B8.pack . show $ err)

main :: IO ()
main = do
    info <- getConnInfo
    sessionSettings <- maybe (fail "Improper session settings") return $
                              H.poolSettings 5 30
    bracket (H.acquirePool info sessionSettings) H.releasePool $ \pool -> do
        _ <- H.session pool $ H.tx Nothing initTable
        scottyT 8080 (fmap hasqlHandler . H.session pool) $ do
                defaultHandler handler
                get "/" $ do
                    l <- lookup "lang" <$> params
                    ps <- lift $ H.tx Nothing $ getPastes (T.unpack <$> l)
                    html $ formatPasteList ps
                get "/:uid/raw" $ do
                    u <- param "uid"
                    p <- lift $ H.tx Nothing $ Tr.mapM getPaste (UUID.fromString u)
                    case join p of
                        Just paste -> text (T.fromStrict $ pasteContent paste)
                        Nothing -> raise NotFound
                get "/:uid" $ do
                    u <- param "uid"
                    style <- param "style" `rescue` const (return "zenburn")
                    p <- lift $ H.tx Nothing $ Tr.mapM getPaste (UUID.fromString u)
                    case join p of
                        Just paste -> html $ formatPaste paste (getStyle style)
                        Nothing -> raise NotFound
                patch "/:uid/:lang" $ do
                    u <- param "uid"
                    l <- param "lang"
                    lift $ H.tx Nothing $ Tr.mapM (`patchPaste` l) (UUID.fromString u)
                    redirect $ T.pack ('/':u)
                post "/:lang" $ do
                    l <- param "lang"
                    c <- body
                    let uid = UUID.generateNamed nsMinipaste (B.unpack c)
                    -- Pray it's actually UTF-8
                    let contents = T.toStrict $ TE.decodeUtf8 c
                    lift $ H.tx Nothing $ postPaste  uid l contents
                    redirect $ T.pack ('/': UUID.toString uid)
