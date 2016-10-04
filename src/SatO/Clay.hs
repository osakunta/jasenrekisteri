{-# LANGUAGE OverloadedStrings #-}
module SatO.Clay where

import Futurice.Prelude hiding (span)
import Prelude ()
import Clay
import qualified Lucid as L

satoCss_ :: Monad m => L.HtmlT m ()
satoCss_ = L.style_ $ render css ^. strict

css :: Css
css = do
    fontSize $ pt 11
    h1 ? do
        satoFontFamily
        fontSize $ pt 15
        marginTop $ em 1
        marginBottom $ em 1
    h2 ? do
        satoFontFamily
        fontSize $ pt 13
        marginTop $ em 1
        marginBottom $ em 1
    td ? satoFontFamily
    li ? satoFontFamily
    a ? satoFontFamily
    span ? satoFontFamily
  where
    satoFontFamily = fontFamily ["Lucida Grande", "Helvetica", "Arial"] [sansSerif]
        
