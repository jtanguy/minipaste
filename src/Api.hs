{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
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

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.ByteString            as B
import           Data.Char
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V5               as UUID
import           Network.Wai
import           Servant
import           Servant.HTML.Blaze

import           Config
import           Database
import           Paste
import           Util

type AppM = ReaderT Config (EitherT ServantErr IO)

type PasteAPI = QueryParam "lang" Lang :> Get '[HTML] [Paste]
            :<|> Capture "pasteid" UUID.UUID :> QueryParam "style" Style :> Get '[HTML] StyledPaste
            :<|> Capture "pasteid" UUID.UUID :> "raw" :> Get '[PlainText] Paste
            :<|> Capture "lang" Lang :> ReqBody '[PlainText] T.Text :> Post '[HTML] Paste
            :<|> "langs" :> Get '[JSON] [Lang]

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
    :<|> pasteRaw
    :<|> create
    :<|> langs


list :: Maybe Lang -> AppM [Paste]
list l = runHasql (getPastes (fmap toText l))

paste :: UUID.UUID -> Maybe Style -> AppM StyledPaste
paste u s = do
    p <- runHasql (getPaste u)
    case (p, s) of
        (Just paste, Nothing) -> return (paste, Haddock)
        (Just paste, Just style) -> return (paste, style)
        (Nothing,_) -> lift $ left err404

pasteRaw :: UUID.UUID -> AppM Paste
pasteRaw u = do
    p <- runHasql (getPaste u)
    case p of
        Just paste -> return paste
        Nothing -> lift $ left err404

create :: Lang -> T.Text -> AppM Paste
create l c = do
    let uid = UUID.generateNamed nsMinipaste (B.unpack (T.encodeUtf8 c))
    runHasql (postPaste uid (toText l) c)

langs :: AppM [Lang]
langs = return [minBound .. maxBound]

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
