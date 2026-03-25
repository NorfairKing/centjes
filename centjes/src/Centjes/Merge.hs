{-# LANGUAGE RecordWildCards #-}

module Centjes.Merge
  ( mergePriceDeclarations,
    extractDeclarationsFromFile,
  )
where

import Centjes.Location
import Centjes.Module
import Centjes.Timestamp (toDay)
import Data.List (sortOn)
import Data.Time (Day)
import Path

-- | Extract declarations belonging to a specific source file from loaded declarations,
-- stripping their SourceSpan annotations.
extractDeclarationsFromFile ::
  Path Rel File ->
  [GenLocated SourceSpan (Declaration SourceSpan)] ->
  [GenLocated () (Declaration ())]
extractDeclarationsFromFile relFile declarations =
  [ noLoc (stripDeclarationAnnotation d)
  | Located loc d <- declarations,
    sourceSpanFile loc == relFile
  ]

-- | Merge new price declarations into existing declarations from a rates file.
-- The original order of existing declarations is preserved.
-- New price declarations are inserted in sorted order among the existing price
-- declarations, so that comments and other declarations stay in place.
mergePriceDeclarations ::
  [GenLocated () (Declaration ())] ->
  [GenLocated () (Declaration ())] ->
  Module ()
mergePriceDeclarations existingDeclarations newPriceDeclarations =
  Module
    { moduleImports = [],
      moduleDeclarations = interleave existingDeclarations (sortOn priceDeclarationSortKey newPriceDeclarations)
    }

-- | Walk through existing declarations in order, inserting new price
-- declarations at the right positions. Non-price declarations stay in place.
-- New prices that sort before the next existing price are inserted before it.
interleave ::
  [GenLocated () (Declaration ())] ->
  [GenLocated () (Declaration ())] ->
  [GenLocated () (Declaration ())]
interleave existing [] = existing
interleave [] new = new
interleave (e : es) new = case locatedValue e of
  DeclarationPrice (Located _ pd) ->
    let key = priceDeclarationKey pd
        (before, after) = span (\n -> priceDeclarationSortKey n < key) new
     in before ++ e : interleave es after
  _ -> e : interleave es new

priceDeclarationKey :: PriceDeclaration () -> (Day, CurrencySymbol)
priceDeclarationKey pd =
  (toDay (locatedValue (priceDeclarationTimestamp pd)), locatedValue (priceDeclarationCurrencySymbol pd))

priceDeclarationSortKey :: GenLocated () (Declaration ()) -> (Day, CurrencySymbol)
priceDeclarationSortKey (Located _ (DeclarationPrice (Located _ pd))) = priceDeclarationKey pd
priceDeclarationSortKey _ = error "priceDeclarationSortKey: not a price declaration"
