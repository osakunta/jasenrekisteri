{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module SatO.Jasenrekisteri.Pages.Search (searchPage, searchCsv, searchXlsx) where

import Prelude ()
import Futurice.Prelude
import Control.Lens         (to)
import Control.Monad.Reader (ask)
import Data.Maybe           (mapMaybe)
import Futurice.IdMap       (key)
import SatO.AcademicYear

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
import qualified Futurice.IdMap  as IdMap

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Contact
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.SearchQuery
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

searchPage :: LoginUser -> Maybe Column -> Maybe SearchQuery' -> QueryM (HtmlPage "search")
searchPage lu mcolumn mquery = do
    (world, today, gcid) <- ask
    pure $ searchPage' gcid today lu world mcolumn mquery

searchPage' :: GoogleClientId -> Day -> LoginUser -> World -> Maybe Column -> Maybe SearchQuery' -> HtmlPage "search"
searchPage' gcid today lu world mcolumn mquery = template' gcid today lu title $ do
    row_ $ large_ 12 $ div_ [ class_ "jrek-search callout secondary" ] $ do
        div_ [ class_ "button-group" ] $ for_ (exampleQueries today) $ \q -> do
            let pq = prettySearchQuery q
            button_ [ data_ "jrek-search-string" pq,  class_ "button" ] $ toHtml pq
        hr_ []
        form_ [ method_ "GET"] $ do
            whenLeft (unwrapSearchQuery' query) $ \(err, _) ->
                div_ [ class_ "callout alert" ] $ pre_ $ toHtml $ T.strip $ err ^. packed
            label_ $ do
                "Haku"
                input_ [ name_ "query", type_ "text", value_ pquery ]
            input_ [ type_ "submit" , value_ "Hae", class_ "button primary" ]
    whenRight (unwrapSearchQuery' query) $ \query' -> do
        row_ $ large_ 12 $ do
            a_ [ searchXlsxHref query' ] "Lataa excelissä osoitteiden kera"
        hr_ []
    memberTagList_ today (\c -> searchHref (Just c) mquery) column world (itoList memberIds)
  where
    title = "Haku: " <> toHtml pquery

    query :: SearchQuery'
    query = fromMaybe (SearchQuery' $ Right $ defaultSearchQuery today) mquery

    pquery :: Text
    pquery = case unwrapSearchQuery' query of
        Left (_, q) -> q
        Right q     -> prettySearchQuery q

    column = case mcolumn of
        Just ColumnRoom | hasTalo -> ColumnRoom
        _ -> ColumnName

    hasTalo = any f memberIds
      where
        f s = Set.member "talo" s || Set.member "osakehuoneisto" s

    memberIds :: Map MemberId (Set TagName)
    memberIds = case unwrapSearchQuery' query of
        Left _       -> Map.empty
        Right query' -> performSearchQuery
            (world ^. worldTagMembers)
            (IdMap.keysSet $ world ^. worldMembers)
            query'

searchCsv :: LoginUser -> Maybe SearchQuery -> QueryM [Contact]
searchCsv _ mquery = do
    (world, today, _) <- ask
    pure $ searchContacts world $ fromMaybe (defaultSearchQuery today) mquery

searchXlsx :: LoginUser -> Maybe SearchQuery -> QueryM SearchResult
searchXlsx _ mquery = do
    (world, today, _) <- ask
    let query = fromMaybe (defaultSearchQuery today) mquery
    pure $ SearchResult query $ searchContacts world query

searchContacts :: World -> SearchQuery -> [Contact]
searchContacts world query = postprocess $ performSearchQuery
    (world ^. worldTagMembers)
    (IdMap.keysSet $ world ^. worldMembers)
    query
  where
    postprocess :: Map MemberId x -> [Contact]
    postprocess m = m ^..
        to Map.keys
        . folded
        . to (\memberId -> world ^. worldMembers . at memberId)
        . folded
        . to contactFromMember

-------------------------------------------------------------------------------
-- Implementation details
-------------------------------------------------------------------------------

defaultSearchQuery :: Day -> SearchQuery
defaultSearchQuery today = QAnd (QOr "talo" "osakehuoneisto") (QNot ayearTag)
  where
    ayear :: Integer
    ayear = academicYear today

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

exampleQueries :: Day -> [SearchQuery]
exampleQueries today =
    [ defaultSearchQuery today
    , QAnd ayearTag (QNot "talo")
    , QOr virk virk'
    , QInterval hallitus hallitus'
    ]
  where
    ayear :: Integer
    ayear = academicYear today

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

    virk :: IsString a => a
    virk = fromString $ "virkailijat" ++ show ayear

    virk' :: IsString a => a
    virk' = fromString $ "virkailijat" ++ show (succ ayear)

    hallitus :: IsString a => a
    hallitus = fromString $ "hallitus" ++ show (pred $ pred ayear)

    hallitus' :: IsString a => a
    hallitus' = fromString $ "hallitus" ++ show (succ ayear)

performSearchQuery
    :: Map TagName (Set MemberId)  -- ^ Tag lookup
    -> Set MemberId                -- ^ All members
    -> SearchQuery
    -> Map MemberId (Set TagName)
       -- ^ Returns members, and an evidence why they are in the result set.
performSearchQuery l a = go
  where
    go (QLiteral tn)     = maybe mempty (literal tn) $ l ^? ix tn
    go (QOr q p)         = Map.unionWith (<>) (go q) (go p)
    go (QAnd q p)        = Map.intersectionWith (<>) (go q) (go p)
    go (QNot q)
        = Map.fromSet (const mempty)
        $ Set.difference a
        $ Map.keysSet
        $ go q
    go (QInterval x y) = Map.unionsWith (<>) $ map p $ itoList l
      where
        p (tn, s)
            | x <= tn && tn <= y = literal tn s
            | otherwise          = Map.empty

    literal tn s = Map.fromSet (const $ Set.singleton tn) s


-------------------------------------------------------------------------------
-- Markup
-------------------------------------------------------------------------------

memberTagList_
    :: Monad m
    => Day
    -> (Column -> Attribute)
    -> Column
    -> World
    -> [(MemberId, Set TagName)]
    -> HtmlT m ()
memberTagList_ today columnHref column world xs = do
    row_ $ do
        largemed_ 6 $ toHtml $  "Yhteensä: " <> (show $ length xs')
        largemed_ 6 $ label_ $ do
            "Suodata: "
            input_ [ type_ "text", id_ "member-filter" ]
    row_ . large_ 12 $ table_ [ id_ "member-list", class_ "hover" ] $ do
        thead_ $ tr_ $ do
            th_ $ a_ [ columnHref ColumnName ] $ "Nimi"
            th_ $ "Tagit"
            th_ ayearTag
            when hasTalo $ th_ $ a_ [ columnHref ColumnRoom ] $ "Huone"
        tbody_ $ for_ xs' $ \(member, tns) -> do
            let memberId = member ^. key
            let needle = T.toLower $ member ^. memberFullName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref memberId ] $ member ^. memberFullNameHtml
                td_ $ tagnameList_ world (tns ^.. folded)
                td_ $ tagCheckbox member ayearTag
                when hasTalo $ td_ $ toHtml $ member ^. memberTaloAddress
  where
    sortOnColumn = case column of
        ColumnName     -> sortOn (view memberSortKey . fst)
        ColumnTags     -> sortOn (view memberSortKey . fst)
        ColumnTagsDesc -> reverse . sortOn (view memberSortKey . fst)
        ColumnRoom     -> sortOn (view memberAddress . fst)

    xs' = sortOnColumn $ mapMaybe lookupMember xs

    lookupMember (memberId, tns) = (,tns) <$> world ^? worldMembers . ix memberId

    -- if 'talo' tag is present, print addresses as well.
    hasTalo = any (f . snd) xs
      where
        f s = Set.member "talo" s || Set.member "osakehuoneisto" s

    ayear :: Integer
    ayear = academicYear today

    ayearTag :: IsString a => a
    ayearTag = fromString $ show ayear ++ "-" ++ show (succ ayear)

-------------------------------------------------------------------------------
-- utilities
-------------------------------------------------------------------------------

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Right _) _ = pure ()
whenLeft (Left x)  f = f x

whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight (Left _)  _ = pure ()
