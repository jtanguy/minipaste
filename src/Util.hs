{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Util
Copyright   : (c) 2015 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Util where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.UUID            as UUID
import qualified Data.UUID.V5         as UUID
import           Servant
import           Text.Read

import           Paste

instance FromText UUID.UUID where
  fromText = UUID.fromString . T.unpack

instance FromText Lang where
  fromText = readMaybe . T.unpack

instance ToText UUID.UUID where
  toText = T.pack . UUID.toString

instance MimeRender PlainText Paste where
  mimeRender _ = B.fromStrict . TE.encodeUtf8 . pasteContent
