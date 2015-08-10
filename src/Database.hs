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
initTable = H.unitEx $ [H.stmt|CREATE TABLE IF NOT EXISTS paste (
                           paste_id uuid primary key,
                           lang Text not null,
                           contents Text not null,
                           created_at timestamptz not null default now())|]

toPaste :: (UUID.UUID,T.Text,T.Text, UTCTime) -> Paste
toPaste (uid, lang, cont, created_at) = Paste uid lang cont created_at

getPaste :: UUID.UUID -> H.Tx H.Postgres s (Maybe Paste)
getPaste uid = fmap (fmap toPaste) <$> H.maybeEx $ [H.stmt|SELECT paste_id, lang, contents, created_at FROM paste WHERE paste_id = ?|] uid

getPastes :: Maybe String -> H.Tx H.Postgres s [Paste]
getPastes (Just l) = fmap (fmap toPaste) <$> H.listEx $ [H.stmt|SELECT paste_id, lang, contents, created_at FROM paste WHERE lang = ?|] l
getPastes Nothing  = fmap (fmap toPaste) <$> H.listEx $ [H.stmt|SELECT paste_id, lang, contents, created_at FROM paste|]


postPaste :: UUID.UUID -> T.Text -> T.Text -> H.Tx H.Postgres s ()
postPaste u l c = do
    p <- getPaste u
    case p of
        Just _ -> return ()
        _ -> H.unitEx $ [H.stmt|INSERT INTO paste (paste_id,lang,contents)
                                        VALUES (?,?,?) |] u l c

patchPaste :: UUID.UUID -> T.Text -> H.Tx H.Postgres s ()
patchPaste uid lang = H.unitEx $ [H.stmt|UPDATE paste SET lang = ? WHERE paste_id = ?|] lang uid

getConnInfo :: IO H.Settings
getConnInfo = do
    uri <- lookupEnv "POSTGRESQL_ADDON_URI"
    case H.StringSettings . B8.pack <$> uri of
        Just p -> return p
        Nothing -> fail "Could not get connection info from environment"
