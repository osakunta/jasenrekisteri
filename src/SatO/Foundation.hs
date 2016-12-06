{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module SatO.Foundation (
    -- * Grid
    row_,
    large_,
    largemed_,
    -- * Form
    optionSelected_,
    checkbox_,
    -- * Page
    HtmlPage (..),
    page_,
    -- * Lucid
    module Lucid,
    attrfor_,
    forWith_,
    ) where

import Prelude ()
import Futurice.Prelude

import Control.Monad.Morph   (hoist)
import Data.FileEmbed        (embedStringFile)
import Data.Functor.Identity (runIdentity)
import Data.Foldable (foldl')
import Data.Swagger          (NamedSchema (..), ToSchema (..))
import GHC.TypeLits          (KnownSymbol, Symbol, symbolVal)
import Lucid                 hiding (for_)
import SatO.Clay             (satoCss_)

import qualified Lucid as L

attrfor_ :: Text -> Attribute
attrfor_ = L.for_

-- | 'intersperse'd 'for_'.
forWith_ :: (Foldable t, Applicative f) => f () -> t a ->  (a -> f b) -> f ()
forWith_ sep xs f = case toList xs of
    []        -> pure ()
    (x : xs') -> foldl' g (void $ f x) xs'
  where
    g = \a b -> a *> sep *> f b *> pure ()

-------------------------------------------------------------------------------
-- Grid
-------------------------------------------------------------------------------

row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ "row"]

large_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
large_ n = div_ [class_ $ fromString $ "columns large-" ++ show n ]

largemed_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
largemed_ n = div_
    [ class_ $ "columns large-" <> textShow n <> " medium-" <> textShow n ]

-------------------------------------------------------------------------------
-- Form
-------------------------------------------------------------------------------

optionSelected_ :: Term arg result => Bool -> arg -> result
optionSelected_ True  = termWith "option" [ selected_ "selected "]
optionSelected_ False = term "option"

checkbox_ :: Monad m => Bool -> [Attribute] -> HtmlT m ()
checkbox_ True  attrs = input_ $ [ type_ "checkbox", checked_ ] <> attrs
checkbox_ False attrs = input_ $ [ type_ "checkbox" ] <> attrs

-------------------------------------------------------------------------------
-- Page
-------------------------------------------------------------------------------

-- TODO: create submodule, move there

newtype HtmlPage (k :: Symbol) = HtmlPage (Html ())

instance KnownSymbol s => ToSchema (HtmlPage s) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ "Html page: " <> name) mempty
      where
        name = symbolVal (Proxy :: Proxy s) ^. packed

instance ToHtml (HtmlPage a) where
    toHtmlRaw = toHtml
    toHtml (HtmlPage h) = hoist (return . runIdentity) h

-------------------------------------------------------------------------------
-- PageParams
-------------------------------------------------------------------------------

page_ :: Html () -> Html () -> HtmlPage k
page_ t b = HtmlPage $ doctypehtml_ $ do
    head_ $ do
        title_  t -- todo: strip tags
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        style_ [type_ "text/css"] ($(embedStringFile "foundation.min.css") :: String)
        style_ [type_ "text/css"] ($(embedStringFile "jquery-ui.min.css") :: String)
        script_ ($(embedStringFile "lodash.fp.min.js") :: Text)
        script_ ($(embedStringFile "menrva.standalone.js") :: Text)
        script_ ($(embedStringFile "jquery-3.1.1.slim.min.js") :: Text)
        script_ ($(embedStringFile "jquery-ui.min.js") :: Text)
        script_ ($(embedStringFile "foundation.min.js") :: Text)
        -- TODO: add fetch
        script_ ($(embedStringFile "jasenrekisteri.js") :: Text)
        satoCss_
    body_ b

