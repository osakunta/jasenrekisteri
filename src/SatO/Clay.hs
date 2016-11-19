{-# LANGUAGE OverloadedStrings #-}
module SatO.Clay where

import Futurice.Prelude hiding (span, (&))
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
    for_ tagColors $ \(cls, col1, col2) ->
        cls ? do
            backgroundColor col1
            color col2
            ":hover" & do
                backgroundColor col2
                color col1
    -- http://stackoverflow.com/questions/2852276/make-div-overlay-entire-page-not-just-viewport
    ".jrek-overlay" & do
      position fixed
      top Clay.nil
      left Clay.nil
      width $ pct 100
      height $ pct 100
      backgroundColor $ rgba 0 0 0 0.5

    ".jrek-message" & do
      marginTop $ em 5

    ".jrek-added" & do
      color "#00cc00"

    ".jrek-removed" & do
      color "#cc0000"

    ".underline" & do
      textDecoration underline

  where
    satoFontFamily = fontFamily ["Lucida Grande", "Helvetica", "Arial"] [sansSerif]
    tagColors =
        [ (".label.lbl0", "#ebebeb", "#4a4a4a")
        , (".label.lbl1", "#990000", "#ffbd98")
        , (".label.lbl2", "#6c4000", "#ffdf81")
        , (".label.lbl3", "#1f5600", "#cafc71")
        , (".label.lbl4", "#005b00", "#91ff77")
        , (".label.lbl5", "#005924", "#8affc8")
        , (".label.lbl6", "#004f80", "#b2f1ff")
        , (".label.lbl7", "#4829c1", "#ffcbff")
        , (".label.lbl8", "#7215a4", "#ffc3ff")
        , (".label.lbl9", "#930061", "#ffbdff")
        ]
