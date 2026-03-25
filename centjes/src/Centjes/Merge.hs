{-# LANGUAGE RecordWildCards #-}

module Centjes.Merge
  ( mergePriceDeclarations,
    extractDeclarationsFromFile,
  )
where

import Centjes.Location
import Centjes.Module
import Centjes.Timestamp (toDay)
import Data.List (partition, sortOn)
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
-- Non-price declarations are kept first (in original order),
-- then all price declarations (existing + new) sorted by (Day, CurrencySymbol).
mergePriceDeclarations ::
  [GenLocated () (Declaration ())] ->
  [GenLocated () (Declaration ())] ->
  Module ()
mergePriceDeclarations existingDeclarations newPriceDeclarations =
  let (existingPriceDeclarations, existingOtherDeclarations) =
        partition (isPriceDeclaration . locatedValue) existingDeclarations
      allDeclarations =
        existingOtherDeclarations
          ++ sortOn priceDeclarationSortKey (existingPriceDeclarations ++ newPriceDeclarations)
   in Module
        { moduleImports = [],
          moduleDeclarations = allDeclarations
        }

isPriceDeclaration :: Declaration ann -> Bool
isPriceDeclaration (DeclarationPrice _) = True
isPriceDeclaration _ = False

priceDeclarationSortKey :: GenLocated () (Declaration ()) -> (Day, CurrencySymbol)
priceDeclarationSortKey (Located _ (DeclarationPrice (Located _ pd))) =
  (toDay (locatedValue (priceDeclarationTimestamp pd)), locatedValue (priceDeclarationCurrencySymbol pd))
priceDeclarationSortKey _ = error "priceDeclarationSortKey: not a price declaration"
