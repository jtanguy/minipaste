{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
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
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Time.Clock
import qualified Data.UUID                  as UUID
import qualified Hasql                      as H
import qualified Hasql.Postgres             as H
import           Servant
import           System.Environment

import           Config
import           Paste

type DBIO a = Config -> IO (Either (H.SessionError H.Postgres) a)

hasqlServantErr :: H.SessionError H.Postgres -> ServantErr
hasqlServantErr e = err500 { errBody = LB8.pack (show e)}

runHasql :: forall a. (forall s. H.Tx H.Postgres s a) -> ReaderT Config (EitherT ServantErr IO) a
runHasql q = ReaderT $ \cfg -> do
    res <- H.session (getPool cfg) $ H.tx Nothing q
    bimapEitherT hasqlServantErr id $ hoistEither res


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

getPastes :: Maybe T.Text -> H.Tx H.Postgres s [Paste]
getPastes (Just l) = fmap (fmap toPaste) <$> H.listEx $ [H.stmt|SELECT paste_id, lang, contents, created_at FROM paste WHERE lang = ?|] l
getPastes Nothing  = fmap (fmap toPaste) <$> H.listEx $ [H.stmt|SELECT paste_id, lang, contents, created_at FROM paste|]

postPaste :: UUID.UUID -> T.Text -> T.Text -> H.Tx H.Postgres s Paste
postPaste u l c = do
    p <- getPaste u
    case p of
        Just paste -> return paste
        Nothing -> fmap toPaste <$> H.singleEx $ [H.stmt|
                                INSERT INTO paste (paste_id,lang,contents)
                                VALUES (?,?,?)
                                RETURNING paste_id,lang,contents,created_at
                               |] u l c

-- patchPaste :: UUID.UUID -> T.Text -> H.Tx H.Postgres s ()
-- patchPaste uid lang = H.unit $ [H.q|UPDATE paste SET lang = ? WHERE paste_id = ?|] lang uid

