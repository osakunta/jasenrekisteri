{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Search (searchPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
import Control.Lens.Att
import Control.Monad.Reader (ask)
import Data.Maybe           (mapMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Futurice.IdMap  as IdMap

import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.SearchQuery
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

searchPage :: LoginUser -> Maybe SearchQuery -> QueryM (HtmlPage "search")
searchPage lu mquery = do
    world <- ask
    pure $ searchPage' lu world mquery

searchPage' :: LoginUser -> World -> Maybe SearchQuery -> HtmlPage "search"
searchPage' lu world mquery = template' lu title $ do
    row_ $ large_ 12 $ div_ [ class_ "jrek-search callout secondary" ] $ do
        div_ [ class_ "button-group" ] $ for_ exampleQueries $ \q -> do
            let pq = prettySearchQuery q
            button_ [ data_ "jrek-search-string" pq,  class_ "button" ] $ toHtml pq
        hr_ []
        form_ [ method_ "GET"] $ do
            label_ $ do
                "Haku"
                input_ [ name_ "query", type_ "text", value_ $ prettySearchQuery query ]
            input_ [ type_ "submit" , value_ "Hae", class_ "button primary" ]
    memberList_ queryTags' $ world ^..
        worldMembers . folded
        . filtered (\member -> personIds ^. contains (member ^. personUuid))
  where
    title = "Haku: " <> toHtml (prettySearchQuery query)

    -- TODO:
    query = fromMaybe defaultSearchQuery mquery

    -- TODO: extract positive tags
    queryTags' = (\tn -> world ^. worldTags . att tn) <$> (queryTags allTags query ^.. folded)
      where
        allTags = Map.keysSet (world ^. worldTagPersons)

    personIds :: Set PersonId
    personIds = performSearchQuery
        (world ^. worldTagPersons)
        (IdMap.keysSet $ world ^. worldMembers)
        query

-- | TODO: take all tags
queryTags :: Set TagName -> SearchQuery -> Set TagName
queryTags _ (QLiteral tn)   = Set.singleton tn
queryTags a (QOr q p)       = Set.union (queryTags a q) (queryTags a p)
queryTags a (QAnd q p)      = Set.union (queryTags a q) (queryTags a p)
queryTags a (QNot q)        = queryTags a q
queryTags a (QInterval x y) = Set.fromList $ filter p $ Set.toList a
  where
    p tn = x <= tn && tn <= y

defaultSearchQuery :: SearchQuery
defaultSearchQuery = QAnd "talo" (QNot "2016-2017")

exampleQueries :: [SearchQuery]
exampleQueries =
    [ defaultSearchQuery
    , QAnd "2016-2017" (QNot "talo")
    , QOr "virkailijat2016" "virkailijat2017"
    , QInterval "hallitus2014" "hallitus2017"
    ]

performSearchQuery
    :: Map TagName (Set PersonId)  -- ^ Tag lookup
    -> Set PersonId                -- ^ All persons
    -> SearchQuery
    -> Set PersonId
performSearchQuery l a = go
  where
    go (QLiteral tn)   = fromMaybe mempty $ l ^? ix tn
    go (QOr q p)       = Set.union (go q) (go p)
    go (QAnd q p)      = Set.intersection (go q) (go p)
    go (QNot q)        = Set.difference a (go q)
    go (QInterval x y) = Set.unions $ mapMaybe p $ itoList l
      where
        p (tn, s)
            | x <= tn && tn <= y = Just s
            | otherwise          = Nothing
