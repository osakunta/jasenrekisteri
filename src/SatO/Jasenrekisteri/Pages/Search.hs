{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SatO.Jasenrekisteri.Pages.Search (searchPage) where

import Control.Lens
import Control.Lens.Att
import Futurice.Prelude
import Prelude ()

import qualified Futurice.IdMap as IdMap
import qualified Data.Set as Set

import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.World
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.Person

searchPage :: World -> HtmlPage "search"
searchPage world = template' title $ do
    row_ $ large_ 12 $ div_ [ class_ "callout secondary" ] $ do
        for_ exampleQueries $ \q -> do
            button_ [ class_ "button" ] $ toHtml $ prettyQuery q
            " "
        hr_ []
        label_ $ do
            "Haku"
            input_ [ type_ "text", value_ $ prettyQuery query ]
        input_ [ type_ "submit" , value_ "Hae", class_ "button primary" ]
    memberList_ queryTags' $ world ^..
        worldMembers . folded
        . filtered (\member -> personIds ^. contains (member ^. personUuid))
  where
    title = "Haku: " <> prettyQuery query

    -- TODO:
    query = defaultQuery

    -- TODO: extract positive tags
    queryTags' = (\tn -> world ^. worldTags . att tn) <$> (queryTags query ^.. folded)

    personIds :: Set PersonId
    personIds = performQuery
        (world ^. worldTagPersons)
        (IdMap.keysSet $ world ^. worldMembers)
        query

queryTags :: Query -> Set TagName
queryTags (QLiteral tn) = Set.singleton tn
queryTags (QOr q p)     = Set.union (queryTags q) (queryTags p)
queryTags (QAnd q p)    = Set.union (queryTags q) (queryTags p)
queryTags (QNot q)      = queryTags q

defaultQuery :: Query
defaultQuery = QAnd
    (QLiteral "talo")
    (QNot (QLiteral "2016-2017"))

exampleQueries :: [Query]
exampleQueries =
    [ defaultQuery
    , QAnd (QLiteral "2016-2017") (QNot (QLiteral "talo"))
    , QOr (QLiteral "virkailijat2016") (QLiteral "virkailijat2017")
    ]

-- | Query is essentially a syntax for boolean / set operations
data Query
    = QLiteral TagName
    | QOr Query Query
    | QAnd Query Query
    | QNot Query
  deriving (Eq, Show)

performQuery
    :: Map TagName (Set PersonId)  -- ^ Tag lookup
    -> Set PersonId                -- ^ All persons
    -> Query
    -> Set PersonId
performQuery l a = go
  where
    go (QLiteral tn) = fromMaybe mempty $ l ^? ix tn
    go (QOr q p)     = Set.union (go q) (go p)
    go (QAnd q p)    = Set.intersection (go q) (go p)
    go (QNot q)      = Set.difference a (go q)

-- /TODO/ avoid unnecessary parentheses
prettyQuery :: Query -> Text
prettyQuery = prettyQuery' defPrec

prettyQuery' :: Int -> Query -> Text
prettyQuery' _d (QLiteral tn) = tn ^. _TagName
prettyQuery'  d (QAnd q p) = parens (d > andPrec) $
    prettyQuery' andPrec q <> " AND " <> prettyQuery' andPrec p
prettyQuery'  d (QOr q p) = parens (d > orPrec) $
    prettyQuery' orPrec p <> " OR " <> prettyQuery' orPrec q
prettyQuery' _d (QNot q) = "NOT " <> prettyQuery' notPrec q

parens :: Bool -> Text -> Text
parens True t  = "(" <> t <> ")"
parens False t = t

notPrec, andPrec, orPrec, defPrec :: Int
notPrec = 3
andPrec = 2
orPrec  = 1
defPrec = 0
