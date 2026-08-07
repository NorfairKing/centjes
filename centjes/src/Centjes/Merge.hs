{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Merge
  ( mergePriceDeclarations,
    mergeTransactionDeclarations,
  )
where

import Centjes.Location
import Centjes.Module
import Centjes.Timestamp (toDay)
import Data.List (sortOn)
import Data.Time (Day)

{-# ANN module ("DisableMutations" :: String) #-}

-- | Merge new price declarations into existing declarations from a rates file.
-- The original order of existing declarations is preserved.
-- New price declarations are inserted in sorted order among the existing price
-- declarations, so that comments and other declarations stay in place.
mergePriceDeclarations ::
  [GenLocated () (Declaration ())] ->
  [GenLocated () (PriceDeclaration ())] ->
  Module ()
mergePriceDeclarations =
  mergeDeclarations
    ( \case
        DeclarationPrice (Located _ pd) -> Just (priceDeclarationSortKey pd)
        _ -> Nothing
    )
    (priceDeclarationSortKey . locatedValue)
    (\(Located _ pd) -> DeclarationPrice (noLoc pd))

-- | Merge new transactions into a file that some importer owns.
--
-- Takes the whole existing module rather than only its declarations, so that a
-- file's imports survive being added to.  Adding a month must not cost the file
-- anything it already said.
--
-- Transactions sort by day only, so several transactions of the same day keep
-- the order they were given in.  An importer that emits a day's transactions in
-- an order its own assertions depend on gets that order back out.
mergeTransactionDeclarations ::
  Module () ->
  [GenLocated () (Transaction ())] ->
  Module ()
mergeTransactionDeclarations existingModule newTransactions =
  let merged =
        mergeDeclarations
          ( \case
              DeclarationTransaction (Located _ t) -> Just (transactionSortKey t)
              _ -> Nothing
          )
          (transactionSortKey . locatedValue)
          (\(Located _ t) -> DeclarationTransaction (noLoc t))
          (moduleDeclarations existingModule)
          newTransactions
   in merged {moduleImports = moduleImports existingModule}

-- | Walk through existing declarations in order, inserting new values at the
-- right positions.
--
-- Declarations that have no sort key stay in place, which is what keeps
-- comments, currency declarations and the like where they were written.  A new
-- value that sorts before the next keyed existing declaration is inserted
-- before it, and one that sorts equal to it goes after it, so that re-running
-- an importer appends rather than interleaves within a day.
mergeDeclarations ::
  forall a key.
  (Ord key) =>
  (Declaration () -> Maybe key) ->
  (a -> key) ->
  (a -> Declaration ()) ->
  [GenLocated () (Declaration ())] ->
  [a] ->
  Module ()
mergeDeclarations existingKey newKey wrap existingDeclarations newValues =
  let interleave ::
        [GenLocated () (Declaration ())] ->
        [a] ->
        [GenLocated () (Declaration ())]
      interleave existing [] = existing
      interleave [] new = map (noLoc . wrap) new
      interleave (e : es) new = case existingKey (locatedValue e) of
        Just key ->
          let (before, after) = span (\n -> newKey n < key) new
           in map (noLoc . wrap) before ++ e : interleave es after
        Nothing -> e : interleave es new
   in Module
        { moduleImports = [],
          moduleDeclarations = interleave existingDeclarations (sortOn newKey newValues)
        }

priceDeclarationSortKey :: PriceDeclaration () -> (Day, CurrencySymbol)
priceDeclarationSortKey pd =
  (toDay (locatedValue (priceDeclarationTimestamp pd)), locatedValue (priceDeclarationCurrencySymbol pd))

transactionSortKey :: Transaction () -> Day
transactionSortKey t = toDay (locatedValue (transactionTimestamp t))
