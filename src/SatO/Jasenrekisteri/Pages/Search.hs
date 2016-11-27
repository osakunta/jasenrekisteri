{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module SatO.Jasenrekisteri.Pages.Search (searchPage, searchCsv) where

import Prelude ()
import Futurice.Prelude
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
import SatO.Jasenrekisteri.Person
import SatO.Jasenrekisteri.SearchQuery
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

searchPage :: LoginUser -> Maybe SearchQuery' -> QueryM (HtmlPage "search")
searchPage lu mquery = do
    (world, today) <- ask
    pure $ searchPage' today lu world mquery

searchPage' :: Day -> LoginUser -> World -> Maybe SearchQuery' -> HtmlPage "search"
searchPage' today lu world mquery = template' today lu title $ do
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
    memberTagList_ today world (itoList personIds)
  where
    title = "Haku: " <> toHtml pquery

    query :: SearchQuery'
    query = fromMaybe (SearchQuery' $ Right $ defaultSearchQuery today) mquery

    pquery :: Text
    pquery = case unwrapSearchQuery' query of
        Left (_, q) -> q
        Right q     -> prettySearchQuery q

    personIds :: Map PersonId (Set TagName)
    personIds = case unwrapSearchQuery' query of
        Left _       -> Map.empty
        Right query' -> performSearchQuery
            (world ^. worldTagPersons)
            (IdMap.keysSet $ world ^. worldMembers)
            query'

searchCsv :: LoginUser -> Maybe SearchQuery -> QueryM [Contact]
searchCsv _ _ = pure []

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
    :: Map TagName (Set PersonId)  -- ^ Tag lookup
    -> Set PersonId                -- ^ All persons
    -> SearchQuery
    -> Map PersonId (Set TagName)
       -- ^ Returns persons, and an evidence why they are in the result set.
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
    -> World
    -> [(PersonId, Set TagName)]
    -> HtmlT m ()
memberTagList_ today world xs = do
    row_ $ do
        largemed_ 6 $ toHtml $  "Yhteensä: " <> (show $ length xs')
        largemed_ 6 $ label_ $ do
            "Suodata: "
            input_ [ type_ "text", id_ "member-filter" ]
    row_ . large_ 12 $ table_ [ id_ "member-list", class_ "hover" ] $ do
        thead_ $ tr_ $ do
            th_ "Nimi"
            th_ "Tagit"
            th_ ayearTag
            when hasTalo $ th_ $ "Huone"
        tbody_ $ for_ xs' $ \(person, tns) -> do
            let memberId = person ^. key
            let needle = T.toLower $ person ^. personFullName
            tr_ [ data_ "member-haystack" needle ] $ do
                td_ $ a_ [ memberHref memberId ] $ person ^. personFullNameHtml
                td_ $ tagnameList_ world (tns ^.. folded)
                td_ $ tagCheckbox person ayearTag
                when hasTalo $ td_ $ toHtml $ modifyAddress $ person ^. personAddress
  where
    xs' = sortOn (view personFullName . fst)
        $ mapMaybe lookupPerson xs

    lookupPerson (memberId, tns) = (,tns) <$> world ^? worldMembers . ix memberId

    -- if 'talo' tag is present, print addresses as well.
    hasTalo = any (f . snd) xs
      where
        f s = Set.member "talo" s || Set.member "osakehuoneisto" s

    -- todo: use regexp
    modifyAddress :: Text -> Text
    modifyAddress = T.replace "Lapinrinne 1 " ""

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
