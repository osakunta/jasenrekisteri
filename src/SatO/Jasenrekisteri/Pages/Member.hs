{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module SatO.Jasenrekisteri.Pages.Member (memberPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens         (filtered, (<&>))
import Control.Monad.Reader (ask)
import Futurice.IdMap       (HasKey (..))
import Text.Printf          (printf)

import qualified Data.UUID    as UUID
import qualified Generics.SOP as SOP

import SatO.Jasenrekisteri.API
import SatO.Jasenrekisteri.Endpoints
import SatO.Jasenrekisteri.Markup
import SatO.Jasenrekisteri.Member
import SatO.Jasenrekisteri.MemberEdit
import SatO.Jasenrekisteri.Session
import SatO.Jasenrekisteri.Tag
import SatO.Jasenrekisteri.World

import qualified Text.Regex.Applicative.Common as RE
import qualified Text.Regex.Applicative.Text   as RE

memberPage :: LoginUser -> MemberId -> QueryM (HtmlPage "member")
memberPage lu memberId = ask <&> \(world, today, gcid) -> case world ^? worldMembers . ix memberId of
    -- TODO: not found page
    Nothing -> page404 gcid today lu
    Just p@Member {..} -> template' gcid today lu (p ^. memberFullNameHtml) $ do
        let pid = p ^. key

        row_ $ large_ 12 $ dl_ $ do
            dt_ "Sähköposti"
            dd_ $ a_ [href_ $ "mailto:" <> _memberEmail] $ toHtml _memberEmail
            dt_ "Puhelin"
            dd_ $ a_ [href_ $ "tel:" <> _memberPhone] $ toHtml _memberPhone
            dt_ "Osoite"
            dd_ $ do
                toHtml _memberAddress
                br_ []
                toHtml $ _memberZipcode <> " " <> _memberCity

        subheader_ "Tagit"
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ do
            tagnameList_ world (world ^.. worldMemberTags . ix pid . _TagNames . folded)
            hr_ []
            row_ [ data_ "jrek-member-tag" $ UUID.toText pid ] $ do
                large_ 6 $ input_ [ type_ "text", placeholder_ "tagi" ]
                large_ 6 $ div_ [ class_ "button-group" ] $ do
                    button_ [ data_ "jrek-action" "add", class_ "button" ] "Lisää"
                    button_ [ data_ "jrek-action" "remove", class_ "button alert" ] "Poista"

        subheader_ "Muokkaa"
        row_ $ large_ 12 $ a_ [ memberlogHref pid ] "Muutosloki"
        hr_ []
        row_ $ large_ 12 $ div_ [ class_ "callout" ] $ div_ [ data_ "jrek-member-edit" $ UUID.toText pid] $ do
            for_ (SOP.hcollapse memberEdits) $ editbox p

            hr_ []
            div_ [ class_ "button-group" ] $ do
                button_ [ data_ "jrek-action" "submit", class_ "button" ] "Tallenna"

        subheader_ "Matrikkeli"
        row_ $ large_ 12 $ do
            let mpage = p ^. memberMatrikkeli
            case parseMatrikkeliPage mpage of
                Nothing -> do
                    toHtml $ mpage <> ": "
                    forWith_ " | " (matrikkeliSamePage world mpage) $ \m' ->
                        a_ [ memberHref $ m' ^. key ] $ m' ^. memberFullNameHtml
                Just (myear, mpage') -> do
                    let prevPage = textMatrikkeliPage myear (mpage' - 1)
                    let nextPage = textMatrikkeliPage myear (mpage' + 1)

                    toHtml $ prevPage <> ": "
                    forWith_ " | " (matrikkeliSamePage world prevPage) $ \m' ->
                        a_ [ memberHref $ m' ^. key ] $ m' ^. memberFullNameHtml
                    br_ []

                    toHtml $ mpage <> ": "
                    forWith_ " | " (matrikkeliSamePage world mpage) $ \m' ->
                        a_ [ memberHref $ m' ^. key ] $ m' ^. memberFullNameHtml
                    br_ []

                    toHtml $ nextPage <> ": "
                    forWith_ " | " (matrikkeliSamePage world nextPage) $ \m' ->
                        a_ [ memberHref $ m' ^. key ] $ m' ^. memberFullNameHtml

-------------------------------------------------------------------------------
-- Same matrikkeli page
-------------------------------------------------------------------------------

matrikkeliSamePage :: World -> Text -> [Member]
matrikkeliSamePage world page =
    world ^.. worldMembers . folded . filtered (\m -> page == m ^. memberMatrikkeli)

parseMatrikkeliPage :: Text -> Maybe (Int, Int)
parseMatrikkeliPage = RE.match regex
  where
    regex = (,) <$> RE.decimal <* "-" <*> RE.decimal

textMatrikkeliPage :: Int -> Int -> Text
textMatrikkeliPage y p = printf "%04d-%04d" y p ^. packed

-------------------------------------------------------------------------------
-- Editbox
-------------------------------------------------------------------------------

editbox :: Member -> PE -> Html ()
editbox p (MkPE i l g _) = label_ $ do
    toHtml l
    input_
        [ type_ "text"
        , data_ "jrek-field-name" i
        , data_ "jrek-field-value" $ p ^. g
        , value_ $ p ^. g
        ]
