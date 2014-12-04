{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-|
Module      : Database
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Database where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.UUID             as UUID

import qualified Hasql                 as H
import qualified Hasql.Postgres        as H
import           System.Environment

import           Paste

-- initTable :: H.Statement H.Postgres
initTable :: H.Tx H.Postgres s ()
initTable = H.unit $ [H.q|CREATE TABLE IF NOT EXISTS paste (paste_id uuid primary key,
                                                   lang Text not null,
                                                   contents Text not null)|]

instance H.RowParser H.Postgres Paste where
    parseRow = fmap toPaste . H.parseRow

toPaste :: (UUID.UUID,T.Text,T.Text) -> Paste
toPaste (uid, lang, cont) = Paste uid lang cont

getPaste :: UUID.UUID -> H.Tx H.Postgres s (Maybe Paste)
getPaste uid = H.single $ [H.q|SELECT paste_id, lang, contents FROM paste WHERE paste_id = ?|] uid

getPastes :: Maybe String -> H.Tx H.Postgres s [Paste]
getPastes (Just l) = H.list $ [H.q|SELECT paste_id, lang, contents FROM paste WHERE lang = ?|] l
getPastes Nothing  = H.list $ [H.q|SELECT paste_id, lang, contents FROM paste|]

-- postPaste :: Paste -> H.Tx H.Postgres s ()
postPaste p@(Paste u l c) = H.unit $ [H.q|insert into paste (paste_id,lang,contents)
                                        select (?,?,?)
                                        where not exists (
                                          select (paste_id,lang,contents) from paste where paste_id = ?
                                        ) |] u l c u

patchPaste :: UUID.UUID -> String -> H.Tx H.Postgres s ()
patchPaste uid lang = H.unit $ [H.q|update paste set lang = ? where paste_id = ?|] lang uid

getConnInfo :: IO H.Postgres
getConnInfo = do
    host <- lookupEnv "POSTGRESQL_ADDON_HOST"
    port <- lookupEnv "POSTGRESQL_ADDON_PORT"
    user <- lookupEnv "POSTGRESQL_ADDON_USER"
    pwd  <- lookupEnv "POSTGRESQL_ADDON_PASSWORD"
    db   <- lookupEnv "POSTGRESQL_ADDON_DB"
    case liftM5 H.Postgres (B8.pack <$> host) (read <$> port) (T.pack <$> user) (T.pack <$> pwd) (T.pack <$> db) of
        Just p -> return p
        Nothing -> fail "Could not get connection info from environment"
