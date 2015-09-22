{-|
Module      : Main
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Minimal pastebin service, powered by Servant, Blaze, and Hasql
-}
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Traversable           as Tr
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V5               as UUID
import qualified Hasql                      as H
import qualified Hasql.Postgres             as H
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server

import           Api
import           Config
import           Database
import           Paste
import           Util


hasqlHandler :: Either (H.SessionError H.Postgres) Response -> Response
hasqlHandler (Right r) = r
hasqlHandler (Left err) = responseLBS status500 [] (B8.pack . show $ err)

main :: IO ()
main = do
  port <- getPortEnv
  info <- getConnInfo
  poolSettings <- getPoolSettings
  bracket (H.acquirePool info poolSettings) H.releasePool $ \pool -> do
    let cfg = Config pool
    eitherT (fail.show) return (runReaderT (runHasql initTable) cfg)
    run port $ app cfg

