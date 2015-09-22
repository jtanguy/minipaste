{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Config
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Config where


import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as B
import qualified Data.UUID                as UUID
import qualified Data.UUID.V5             as UUID
import qualified Hasql                    as H
import qualified Hasql.Postgres           as H
import           Network.Wai.Handler.Warp
import           System.Environment

nsMinipaste :: UUID.UUID
nsMinipaste = UUID.generateNamed UUID.namespaceURL (B.unpack "github.com/jtanguy/minipaste")

getPortEnv :: IO Port
getPortEnv = maybe 8080 read <$> lookupEnv "PORT"

data Config = Config {
    getPool :: H.Pool H.Postgres
    }

getConnInfo :: IO H.Settings
getConnInfo = do
    uri <- lookupEnv "POSTGRESQL_ADDON_URI"
    case H.StringSettings . B8.pack <$> uri of
        Just p -> return p
        Nothing -> fail "Could not get connection info from environment"

getPoolSettings :: IO H.PoolSettings
getPoolSettings = maybe (fail "Improper session settings") return $
    H.poolSettings 5 30
