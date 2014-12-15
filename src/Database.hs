{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-|
Module      : Database
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable


Database abstractions
-}
module Database where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Time.Clock
import qualified Data.UUID             as UUID
import qualified Hasql                 as H
import qualified Hasql.Postgres        as H
import           System.Environment

import           Paste

initTable :: H.Tx H.Postgres s ()
initTable = H.unit $ [H.q|CREATE TABLE IF NOT EXISTS paste (
                               paste_id uuid primary key,
                               lang Text not null,
                               contents Text not null,
                               created_at timestamptz not null default now())|]

instance H.RowParser H.Postgres Paste where
    parseRow = fmap toPaste . H.parseRow

toPaste :: (UUID.UUID,T.Text,T.Text, UTCTime) -> Paste
toPaste (uid, lang, cont, created_at) = Paste uid lang cont created_at

getPaste :: UUID.UUID -> H.Tx H.Postgres s (Maybe Paste)
getPaste uid = H.single $ [H.q|SELECT paste_id, lang, contents, created_at FROM paste WHERE paste_id = ?|] uid

getPastes :: Maybe String -> H.Tx H.Postgres s [Paste]
getPastes (Just l) = H.list $ [H.q|SELECT paste_id, lang, contents, created_at FROM paste WHERE lang = ?|] l
getPastes Nothing  = H.list $ [H.q|SELECT paste_id, lang, contents, created_at FROM paste|]

postPaste :: UUID.UUID -> T.Text -> T.Text -> H.Tx H.Postgres s ()
postPaste u l c = do
    p <- getPaste u
    case p of
        Just _ -> return ()
        _ -> H.unit $ [H.q|INSERT INTO paste (paste_id,lang,contents)
                                        VALUES (?,?,?) |] u l c

patchPaste :: UUID.UUID -> T.Text -> H.Tx H.Postgres s ()
patchPaste uid lang = H.unit $ [H.q|UPDATE paste SET lang = ? WHERE paste_id = ?|] lang uid

getConnInfo :: IO H.Postgres
getConnInfo = do
    host <- lookupEnv "POSTGRESQL_ADDON_HOST"
    port <- lookupEnv "POSTGRESQL_ADDON_PORT"
    user <- lookupEnv "POSTGRESQL_ADDON_USER"
    pwd  <- lookupEnv "POSTGRESQL_ADDON_PASSWORD"
    db   <- lookupEnv "POSTGRESQL_ADDON_DB"
    case liftM5 H.ParamSettings (B8.pack <$> host) (read <$> port) (B8.pack <$> user) (B8.pack <$> pwd) (B8.pack <$> db) of
        Just p -> return p
        Nothing -> fail "Could not get connection info from environment"
