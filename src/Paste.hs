{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Paste
Copyright   : (c) 2014 Julien Tanguy
License     : BSD3

Maintainer  : julien.tanguy@jhome.fr
Stability   : experimental
Portability : portable

Paste definitions and views
-}
module Paste where

import           Control.Monad
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.UUID                     as UUID
import           System.Locale hiding (defaultTimeLocale)
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              as H hiding (param)
import           Text.Blaze.Html5.Attributes   as A
import           Text.Highlighting.Kate

data Paste = Paste { pasteId      :: UUID.UUID
                   , pasteLang    :: T.Text
                   , pasteContent :: T.Text
                   , created_at   :: UTCTime
                   } deriving (Show)

getStyle :: String -> Style
getStyle "pygments" = pygments
getStyle "kate" = kate
getStyle "espresso" = espresso
getStyle "tango" = tango
getStyle "haddock" = haddock
getStyle "monochrome" = monochrome
getStyle "zenburn" = zenburn
getStyle _ = monochrome

formatPaste :: Paste -> Style -> TL.Text
formatPaste (Paste _ lang code _) style = renderHtml $ do
    H.head $ H.style ! A.type_ (toValue ("text/css" :: String))
           $ toHtml $ styleToCss style
    H.body $ toHtml
           $ formatHtmlBlock defaultFormatOpts{numberLines=True}
           $ highlightAs (T.unpack lang) (T.unpack code)

formatPasteList :: [Paste] -> TL.Text
formatPasteList pastes = renderHtml $ do
    H.body $ toHtml $ H.table $ do
           H.thead $ H.tr $ sequence_ [td "Paste" , td "Lang", td "Created at"]
           H.tbody $ forM_ pastes pasteLine
  where
    pasteLine (Paste u l _ c) = tr $ do
        td $ a ! href (toValue $ UUID.toString u) $ toHtml (UUID.toString u)
        td $ toHtml l
        td $ toHtml $ formatTime defaultTimeLocale "%F %T %Z" c
