{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeOperators     #-}
{-|
Module      : Api
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Api where

import Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Servant
import           Servant.HTML.Blaze
import qualified Data.UUID                as UUID
import qualified Data.UUID.V5             as UUID
import           Network.Wai

import Config
import Database
import Paste
import Util

type AppM = ReaderT Config (EitherT ServantErr IO)

type PasteAPI = QueryParam "lang" Lang :> Get '[HTML] [Paste]
            :<|> Capture "pasteid" UUID.UUID :> Get '[HTML, PlainText] Paste
            -- :<|> Capture "lang" Lang :> ReqBody '[FormUrlEncoded] Paste :> Post '[HTML] Paste

pasteAPI :: Proxy PasteAPI
pasteAPI = Proxy

app :: Config -> Application
app cfg = serve pasteAPI (readerServer cfg)

readerServer :: Config -> Server PasteAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT PasteAPI AppM
server = list
    :<|> paste
    -- :<|> create


list :: Maybe Lang -> AppM [Paste]
list l = runHasql (getPastes (fmap show l))

paste :: UUID.UUID -> AppM Paste
paste u = do
    p <- runHasql (getPaste u)
    case p of
        Just paste -> return paste
        Nothing -> lift $ left err404

-- main :: IO ()
-- main = do
--     info <- getConnInfo
--     sessionSettings <- maybe (fail "Improper session settings") return $
--                               H.sessionSettings 5 30
--     H.session info sessionSettings $ do
--         H.tx Nothing initTable
--         sess <- H.sessionUnlifter
--         scottyT 8080 id sess $ do
--             defaultHandler handler
--             get "/" $ do
--                 l <- lookup "lang" <$> params
--                 ps <- lift $ H.tx Nothing $ getPastes (T.unpack <$> l)
--                 html $ formatPasteList ps
--             get "/:uid/raw" $ do
--                 u <- param "uid"
--                 p <- lift $ H.tx Nothing $ Tr.mapM getPaste (UUID.fromString u)
--                 case join p of
--                     Just paste -> text (T.fromStrict $ pasteContent paste)
--                     Nothing -> raise NotFound
--             get "/:uid" $ do
--                 u <- param "uid"
--                 style <- param "style" `rescue` const (return "zenburn")
--                 p <- lift $ H.tx Nothing $ Tr.mapM getPaste (UUID.fromString u)
--                 case join p of
--                     Just paste -> html $ formatPaste paste (getStyle style)
--                     Nothing -> raise NotFound
--             patch "/:uid/:lang" $ do
--                 u <- param "uid"
--                 l <- param "lang"
--                 lift $ H.tx Nothing $ Tr.mapM (`patchPaste` l) (UUID.fromString u)
--                 redirect $ T.pack ('/':u)
--             post "/:lang" $ do
--                 l <- param "lang"
--                 c <- body
--                 let uid = UUID.generateNamed nsMinipaste (B.unpack c)
--                 -- Pray it's actually UTF-8
--                 let contents = T.toStrict $ TE.decodeUtf8 c
--                 lift $ H.tx Nothing $ postPaste  uid l contents
--                 redirect $ T.pack ('/': UUID.toString uid)
