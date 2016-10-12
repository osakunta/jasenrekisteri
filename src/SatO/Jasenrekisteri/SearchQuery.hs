{-# LANGUAGE OverloadedStrings #-}
module SatO.Jasenrekisteri.SearchQuery where

import Futurice.Prelude
import Prelude ()
import Text.Trifecta

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
searchQueryParser = pure (QLiteral "foo")
