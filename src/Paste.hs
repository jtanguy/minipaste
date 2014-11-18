{-#LANGUAGE OverloadedStrings#-}
{-|
Module      : Paste
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable



-}
module Paste where

import           Control.Monad
import qualified Data.ByteString.Lazy               as B
import qualified Data.ByteString.Lazy.Char8         as B8
import qualified Data.Text.Lazy                     as T
import qualified Data.UUID                          as UUID
import           Text.Blaze.Html                    (toHtml)
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import           Text.Blaze.Html5                   as H hiding (param)
import           Text.Blaze.Html5.Attributes        as A
import           Text.Highlighting.Kate

data Paste = Paste { pasteId      :: UUID.UUID
                   , pasteLang    :: String
                   , pasteContent :: B.ByteString
                   } deriving (Show)

formatPaste :: Paste -> T.Text
formatPaste (Paste _ lang code) = renderHtml $ do
    H.head $ H.style ! A.type_ (toValue ("text/css" :: String))
           $ toHtml $ styleToCss zenburn
    H.body $ toHtml
           $ formatHtmlBlock defaultFormatOpts{numberLines=True}
           $ highlightAs lang (B8.unpack code)

formatPasteList :: [Paste] -> T.Text
formatPasteList pastes = renderHtml $ do
    H.body $ toHtml $ H.table $ do
           H.thead $ H.tr $ sequence_ [td "paste" , td "lang"]
           H.tbody $ forM_ pastes pasteLine
  where
    pasteLine (Paste u l _) = tr $ do
        td $ a ! href (toValue $ UUID.toString u) $ toHtml (UUID.toString u)
        td $ toHtml l
