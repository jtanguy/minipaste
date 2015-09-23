{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.Lazy              as TL
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.UUID                   as UUID
import           GHC.Generics
import           Language.Haskell.TH
import           System.Locale               hiding (defaultTimeLocale)
import           Text.Blaze.Html5            as H hiding (param)
import           Text.Blaze.Html5.Attributes as A
import qualified Text.Highlighting.Kate      as Kate

data Paste = Paste { pasteId      :: UUID.UUID
                   , pasteLang    :: T.Text
                   , pasteContent :: T.Text
                   , createdAt    :: UTCTime
                   } deriving (Eq, Show, Generic)

$(return <$> dataD (cxt []) (mkName "Lang") [] (fmap (\n -> normalC (mkName n) []) Kate.languages) [''Eq, ''Show, ''Read, ''Generic])

data Style = Pygments | Kate | Espresso | Tango | Haddock | Monochrome | Zenburn
  deriving (Eq, Show, Read, Generic)

type StyledPaste = (Paste, Style)

getStyle :: Style -> Kate.Style
getStyle Pygments = Kate.pygments
getStyle Kate = Kate.kate
getStyle Espresso = Kate.espresso
getStyle Tango = Kate.tango
getStyle Haddock = Kate.haddock
getStyle Monochrome = Kate.monochrome
getStyle Zenburn = Kate.zenburn

instance H.ToMarkup Paste where
  toMarkup (Paste _ lang code _) = Kate.formatHtmlBlock Kate.defaultFormatOpts{Kate.numberLines=True}
                                 $ Kate.highlightAs (T.unpack lang) (T.unpack code)

instance H.ToMarkup StyledPaste where
  toMarkup (p,s) = do
    H.head $ H.style ! A.type_ (toValue ("text/css" :: String))
           $ toHtml $ Kate.styleToCss (getStyle s)
    H.body $ toMarkup p

instance H.ToMarkup [Paste] where
  toMarkup ps = do
    H.head $ H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://groundfloor.neocities.org/default.css"
    H.body $ H.table $ do
           H.thead $ H.tr $ sequence_ [th "Paste" , th "Lang", th "Created at"]
           H.tbody $ forM_ ps pasteLine
    where
      pasteLine (Paste u l _ c) = H.tr $ do
        H.td $ H.a ! href (toValue $ UUID.toString u) $ toHtml (UUID.toString u)
        H.td $ H.a ! href (toValue ("/?lang=" ++ T.unpack l) ) $ toHtml l
        H.td $ toHtml $ formatTime defaultTimeLocale "%F %T %Z" c



-- formatPaste :: Paste -> Style -> TL.Text
-- formatPaste (Paste _ lang code _) style = renderHtml $ do
--     H.head $ H.style ! A.type_ (toValue ("text/css" :: String))
--            $ toHtml $ styleToCss style
--     H.body $ toHtml
--            $ formatHtmlBlock defaultFormatOpts{numberLines=True}
--            $ highlightAs (T.unpack lang) (T.unpack code)

-- formatPasteList :: [Paste] -> TL.Text
-- formatPasteList pastes = renderHtml $ do
--     H.body $ toHtml $ H.table $ do
--            H.thead $ H.tr $ sequence_ [td "Paste" , td "Lang", td "Created at"]
--            H.tbody $ forM_ pastes pasteLine
--   where
--     pasteLine (Paste u l _ c) = tr $ do
--         td $ a ! href (toValue $ UUID.toString u) $ toHtml (UUID.toString u)
--         td $ toHtml l
--         td $ toHtml $ formatTime defaultTimeLocale "%F %T %Z" c
