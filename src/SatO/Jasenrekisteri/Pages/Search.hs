{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Search (searchPage) where

import Control.Lens
import Control.Lens.Att
import Futurice.Prelude
import Control.Monad.Reader (ask)
import Prelude ()

import qualified Data.Set       as Set
import qualified Futurice.IdMap as IdMap

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
    row_ $ large_ 12 $ div_ [ class_ "callout secondary" ] $ do
        for_ exampleQueries $ \q -> do
            button_ [ class_ "button" ] $ toHtml $ prettySearchQuery q
            " "
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
    title = "Haku: " <> prettySearchQuery query

    -- TODO:
    query = fromMaybe defaultSearchQuery mquery

    -- TODO: extract positive tags
    queryTags' = (\tn -> world ^. worldTags . att tn) <$> (queryTags query ^.. folded)

    personIds :: Set PersonId
    personIds = performSearchQuery
        (world ^. worldTagPersons)
        (IdMap.keysSet $ world ^. worldMembers)
        query

queryTags :: SearchQuery -> Set TagName
queryTags (QLiteral tn) = Set.singleton tn
queryTags (QOr q p)     = Set.union (queryTags q) (queryTags p)
queryTags (QAnd q p)    = Set.union (queryTags q) (queryTags p)
queryTags (QNot q)      = queryTags q

defaultSearchQuery :: SearchQuery
defaultSearchQuery = QAnd
    (QLiteral "talo")
    (QNot (QLiteral "2016-2017"))

exampleQueries :: [SearchQuery]
exampleQueries =
    [ defaultSearchQuery
    , QAnd (QLiteral "2016-2017") (QNot (QLiteral "talo"))
    , QOr (QLiteral "virkailijat2016") (QLiteral "virkailijat2017")
    ]

performSearchQuery
    :: Map TagName (Set PersonId)  -- ^ Tag lookup
    -> Set PersonId                -- ^ All persons
    -> SearchQuery
    -> Set PersonId
performSearchQuery l a = go
  where
    go (QLiteral tn) = fromMaybe mempty $ l ^? ix tn
    go (QOr q p)     = Set.union (go q) (go p)
    go (QAnd q p)    = Set.intersection (go q) (go p)
    go (QNot q)      = Set.difference a (go q)
