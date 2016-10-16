{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.SearchQuery where

import Prelude ()
import Futurice.Prelude
import Data.ByteString     (ByteString)
import Text.Trifecta
import Text.Trifecta.Delta (Delta (Directed))
import Web.HttpApiData     (FromHttpApiData (..))

import qualified Data.Text.Encoding           as TE
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import SatO.Jasenrekisteri.Tag

-- | SearchQuery is essentially a syntax for boolean / set operations
data SearchQuery
    = QLiteral TagName
    | QOr SearchQuery SearchQuery
    | QAnd SearchQuery SearchQuery
    | QNot SearchQuery
  deriving (Eq, Show)

qOr :: SearchQuery -> SearchQuery -> SearchQuery
qOr = QOr

qAnd :: SearchQuery -> SearchQuery -> SearchQuery
qAnd = QAnd

qNot :: SearchQuery -> SearchQuery
qNot (QNot q)       = q
qNot (QOr q p)      = QAnd (qNot q) (qNot p)
qNot (QAnd q p)     = QOr (qNot q) (qNot p)
qNot q@(QLiteral _) = QNot q

-- /TODO/ avoid unnecessary parentheses
prettySearchQuery :: SearchQuery -> Text
prettySearchQuery = prettySearchQuery' defPrec

prettySearchQuery' :: Int -> SearchQuery -> Text
prettySearchQuery' _d (QLiteral tn) = tn ^. _TagName
prettySearchQuery'  d (QAnd q p) = prettyParens (d > andPrec) $
    prettySearchQuery' andPrec q <> " AND " <> prettySearchQuery' andPrec p
prettySearchQuery'  d (QOr q p) = prettyParens (d > orPrec) $
    prettySearchQuery' orPrec q <> " OR " <> prettySearchQuery' orPrec p
prettySearchQuery' _d (QNot q) = "NOT " <> prettySearchQuery' notPrec q

prettyParens :: Bool -> Text -> Text
prettyParens True t  = "(" <> t <> ")"
prettyParens False t = t

notPrec, andPrec, orPrec, defPrec :: Int
notPrec = 3
andPrec = 2
orPrec  = 1
defPrec = 0

searchQueryParser :: Parser SearchQuery
searchQueryParser = queryP
  where
    queryP    = orP

    orP       = foldl QOr <$> andP <*> many (stringP "OR" *> andP)
    andP      = foldl QAnd <$> notP <*> many (stringP "AND" *> notP)
    notP      = maybe id (\_ -> QNot) <$> optional (stringP "NOT") <*> litP

    litP      = litP' <|> parensP queryP
    litP'     = QLiteral . fromString <$> some (alphaNum <|> oneOf "-") <* spaces <?> "Tagin nimi"

    stringP s = string s <* spacesP
    spacesP   = space *> spaces -- at least one space
    parensP   = between (char '(' *> spaces) (char ')' *> spaces)

parseSearchQuery :: ByteString -> Either String SearchQuery
parseSearchQuery bs =
    case parseByteString (spaces *> searchQueryParser <* eof) (Directed "<input>" 0 0 0 0) bs of
        Success q -> Right q
        Failure e -> Left $ PP.displayS (PP.renderCompact  $ _errDoc e) ""

instance FromHttpApiData SearchQuery where
    parseUrlPiece = first (view packed) . parseSearchQuery . TE.encodeUtf8
