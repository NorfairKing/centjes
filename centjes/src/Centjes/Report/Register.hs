{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Register report production.
--
-- The register is produced in three stages:
--
-- 1. __Flat register__ ('produceFlatRegister'): Process transactions
--    chronologically to build a flat list of entries. Each entry contains
--    the transaction data, running totals in original currencies, and the
--    price graph at that point in time.
--
-- 2. __Conversion__ ('convertFlatRegister', optional): Convert all amounts
--    to a target currency ('SingleCurrency' mode). This stage uses the
--    stored price graphs to convert amounts and computes revaluations when
--    prices change. The GADT 'RegisterEntry' enforces at the type level
--    that revaluations can only exist in single-currency registers.
--
-- 3. __Block grouping__ ('groupMultiIntoBlocks', 'groupSingleIntoBlocks'):
--    Group entries into time periods (daily, weekly, monthly, etc.) and
--    compute running totals and averages per block.
--
-- The unified entry point 'produceRegister' dispatches to either path based
-- on whether a target currency is specified.
--
-- The type parameter @mode :: AmountMode@ distinguishes between:
--
-- * @'MultiCurrency@: Amounts are 'Money.MultiAccount' (multiple currencies)
-- * @'SingleCurrency@: Amounts are 'Money.Account' (single target currency)
module Centjes.Report.Register
  ( -- * Amount modes
    AmountMode (..),
    AmountOf,

    -- * Block-grouped registers (final output)
    Register (..),
    RegisterBlock (..),
    RegisterEntry (..),
    RegisterTransaction (..),
    RegisterRevaluation (..),
    RegisterPosting (..),
    ConvertedRegister (..),
    AnyRegister (..),

    -- * Flat registers (intermediate)
    FlatRegister (..),
    FlatEntry (..),
    FlatPosting (..),
    ConvertedFlatRegister (..),
    ConvertedFlatEntry (..),
    ConvertedFlatPosting (..),
    ConvertedFlatTransaction (..),
    ConvertedFlatRevaluation (..),

    -- * Configuration
    BlockSize (..),

    -- * Errors
    RegisterError (..),

    -- * Production functions
    produceRegister,
    produceMultiCurrencyRegister,
    produceConvertedRegister,

    -- * Stage 1: Flat register production
    produceFlatRegister,

    -- * Stage 2: Conversion
    convertFlatRegister,

    -- * Stage 3: Block grouping
    groupMultiIntoBlocks,
    groupSingleIntoBlocks,

    -- * Re-exports for function argument types
    Filter (..),
    CurrencySymbol,
    Currency (..),
    Ledger (..),
    Price (..),
    GenLocated (..),
  )
where

import Centjes.Block
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Filter (Filter (..))
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location (GenLocated (..), SourceSpan)
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation (ToReport (..), Validation (..), mapValidationFailure, validationFailure)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Validity (Validity (..), declare)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account, Rounding (..))
import qualified Money.MultiAccount as Money (MultiAccount (..))
import qualified Money.MultiAccount as MultiAccount

-- | Amount mode distinguishes between multi-currency and single-currency registers
data AmountMode = MultiCurrency | SingleCurrency

-- | Type family mapping amount mode to the actual amount type
type family AmountOf (mode :: AmountMode) ann where
  AmountOf 'MultiCurrency ann = Money.MultiAccount (Currency ann)
  AmountOf 'SingleCurrency ann = Money.Account

-- Flat register types (Stage 1 output)

-- | A flat register: a list of entries with running totals and price info.
-- This is the output of Stage 1, before block grouping.
data FlatRegister ann = FlatRegister
  { flatRegisterEntries :: ![FlatEntry ann],
    flatRegisterTotal :: !(Money.MultiAccount (Currency ann))
  }

deriving instance (Show ann) => Show (FlatRegister ann)

-- | A flat entry: a transaction with its postings, price graph, and running total.
data FlatEntry ann = FlatEntry
  { flatEntryTimestamp :: !(GenLocated ann Timestamp),
    flatEntryDescription :: !(Maybe (GenLocated ann Description)),
    flatEntryPostings :: !(Vector (FlatPosting ann)),
    -- | The price graph at the time of this entry (for later conversion)
    flatEntryPriceGraph :: !(MemoisedPriceGraph (Currency ann)),
    -- | Running total after this entry (in original currencies)
    flatEntryRunningTotal :: !(Money.MultiAccount (Currency ann))
  }

deriving instance (Show ann) => Show (FlatEntry ann)

-- | A flat posting with its amount and running total.
data FlatPosting ann = FlatPosting
  { flatPosting :: !(GenLocated ann (Posting ann)),
    flatPostingAmount :: !(Money.MultiAccount (Currency ann)),
    flatPostingRunningTotal :: !(Money.MultiAccount (Currency ann))
  }

deriving instance (Show ann) => Show (FlatPosting ann)

-- Converted flat register types (Stage 2 output)

-- | A converted flat register: entries converted to a single currency.
-- May include revaluation entries where prices changed.
data ConvertedFlatRegister ann = ConvertedFlatRegister
  { convertedFlatRegisterCurrency :: !(Currency ann),
    convertedFlatRegisterEntries :: ![ConvertedFlatEntry ann],
    convertedFlatRegisterTotal :: !Money.Account
  }

deriving instance (Show ann) => Show (ConvertedFlatRegister ann)

-- | A converted flat entry: either a transaction or a revaluation.
data ConvertedFlatEntry ann
  = ConvertedFlatEntryTransaction !(ConvertedFlatTransaction ann)
  | ConvertedFlatEntryRevaluation !(ConvertedFlatRevaluation ann)

deriving instance (Show ann) => Show (ConvertedFlatEntry ann)

-- | A converted transaction with single-currency amounts.
data ConvertedFlatTransaction ann = ConvertedFlatTransaction
  { convertedFlatTransactionTimestamp :: !(GenLocated ann Timestamp),
    convertedFlatTransactionDescription :: !(Maybe (GenLocated ann Description)),
    convertedFlatTransactionPostings :: !(Vector (ConvertedFlatPosting ann)),
    convertedFlatTransactionRunningTotal :: !Money.Account
  }

deriving instance (Show ann) => Show (ConvertedFlatTransaction ann)

-- | A converted posting with single-currency amount.
data ConvertedFlatPosting ann = ConvertedFlatPosting
  { convertedFlatPosting :: !(GenLocated ann (Posting ann)),
    convertedFlatPostingAmount :: !Money.Account,
    convertedFlatPostingRunningTotal :: !Money.Account
  }

deriving instance (Show ann) => Show (ConvertedFlatPosting ann)

-- | A revaluation entry from price changes.
data ConvertedFlatRevaluation ann = ConvertedFlatRevaluation
  { convertedFlatRevaluationTimestamp :: !(GenLocated ann Timestamp),
    convertedFlatRevaluationCurrency :: !(GenLocated ann (Currency ann)),
    convertedFlatRevaluationAmount :: !Money.Account,
    convertedFlatRevaluationRunningTotal :: !Money.Account
  }

deriving instance (Show ann) => Show (ConvertedFlatRevaluation ann)

-- Block-grouped register types (Stage 3 output)

-- | A register parameterized by amount mode
data Register (mode :: AmountMode) ann = Register
  { registerBlockSize :: !BlockSize,
    registerBlocks :: !(Vector (RegisterBlock mode ann)),
    registerTotal :: !(AmountOf mode ann)
  }

deriving instance (Show ann, Show (AmountOf mode ann)) => Show (Register mode ann)

instance (Validity ann, Eq ann, Ord ann) => Validity (Register 'MultiCurrency ann) where
  validate Register {..} =
    mconcat
      [ validate registerBlockSize,
        foldMap validate registerBlocks,
        declare "The total of the register matches the sum of the blocks" $
          MultiAccount.sum (fmap registerBlockTotal registerBlocks)
            == Just registerTotal
      ]

instance (Validity ann, Show ann, Ord ann) => Validity (Register 'SingleCurrency ann) where
  validate Register {..} =
    mconcat
      [ validate registerBlockSize,
        foldMap validate registerBlocks,
        declare "The total of the register matches the sum of the blocks" $
          foldl (\macc b -> macc >>= Account.add (registerBlockTotal b)) (Just Account.zero) registerBlocks
            == Just registerTotal
      ]

data RegisterBlock (mode :: AmountMode) ann = RegisterBlock
  { registerBlockTitle :: !Block,
    registerBlockEntries :: !(Vector (RegisterEntry mode ann)),
    -- Total of this block
    registerBlockTotal :: !(AmountOf mode ann),
    -- Running accross blocks
    registerBlockRunningTotal :: !(AmountOf mode ann),
    -- Running average accross blocks
    registerBlockRunningAverage :: !(AmountOf mode ann)
  }

deriving instance (Show ann, Show (AmountOf mode ann)) => Show (RegisterBlock mode ann)

instance (Validity ann, Eq ann, Ord ann) => Validity (RegisterBlock 'MultiCurrency ann) where
  validate RegisterBlock {..} =
    mconcat
      [ validate registerBlockTitle,
        foldMap validate registerBlockEntries,
        declare "The total of the block matches the sum of the entries" $
          (mapM registerEntryTotalMulti registerBlockEntries >>= MultiAccount.sum)
            == Just registerBlockTotal
      ]

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterBlock 'SingleCurrency ann) where
  validate RegisterBlock {..} =
    mconcat
      [ validate registerBlockTitle,
        foldMap validate registerBlockEntries,
        declare "The total of the block matches the sum of the entries" $
          foldl (\macc e -> macc >>= Account.add (registerEntryTotalSingle e)) (Just Account.zero) registerBlockEntries
            == Just registerBlockTotal
      ]

-- | A register entry is either a transaction or a revaluation
-- Revaluations can only exist in SingleCurrency mode (GADT constraint)
data RegisterEntry (mode :: AmountMode) ann where
  RegisterEntryTransaction ::
    !(RegisterTransaction mode ann) ->
    RegisterEntry mode ann
  -- Revaluations ONLY exist in SingleCurrency mode
  RegisterEntryRevaluation ::
    !(RegisterRevaluation ann) ->
    RegisterEntry 'SingleCurrency ann

deriving instance (Show ann, Show (AmountOf mode ann)) => Show (RegisterEntry mode ann)

instance (Validity ann, Eq ann) => Validity (RegisterEntry 'MultiCurrency ann) where
  validate (RegisterEntryTransaction rt) = validate rt

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterEntry 'SingleCurrency ann) where
  validate (RegisterEntryTransaction rt) = validate rt
  validate (RegisterEntryRevaluation rr) = validate rr

-- | Get the total amount for a multi-currency register entry
registerEntryTotalMulti ::
  (Ord ann) =>
  RegisterEntry 'MultiCurrency ann ->
  Maybe (Money.MultiAccount (Currency ann))
registerEntryTotalMulti (RegisterEntryTransaction rt) = registerTransactionTotalMulti rt

-- | Get the total amount for a single-currency register entry
registerEntryTotalSingle ::
  RegisterEntry 'SingleCurrency ann ->
  Money.Account
registerEntryTotalSingle = \case
  RegisterEntryTransaction rt -> registerTransactionTotalSingle rt
  RegisterEntryRevaluation rr -> registerRevaluationAmount rr

-- | A revaluation entry representing gain/loss due to price changes
-- Always uses single-currency amounts
data RegisterRevaluation ann = RegisterRevaluation
  { registerRevaluationTimestamp :: !(GenLocated ann Timestamp),
    -- | The currency whose price changed
    registerRevaluationCurrency :: !(GenLocated ann (Currency ann)),
    -- | The gain or loss amount
    registerRevaluationAmount :: !Money.Account,
    -- | Running total across blocks after this revaluation
    registerRevaluationRunningTotal :: !Money.Account,
    -- | Running total within the block after this revaluation
    registerRevaluationBlockRunningTotal :: !Money.Account
  }
  deriving (Generic)

deriving instance (Show ann) => Show (RegisterRevaluation ann)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterRevaluation ann)

data RegisterTransaction (mode :: AmountMode) ann = RegisterTransaction
  { registerTransactionTimestamp :: !(GenLocated ann Timestamp),
    registerTransactionDescription :: !(Maybe (GenLocated ann Description)),
    registerTransactionPostings :: !(Vector (RegisterPosting mode ann))
  }

deriving instance (Show ann, Show (AmountOf mode ann)) => Show (RegisterTransaction mode ann)

instance (Validity ann, Eq ann) => Validity (RegisterTransaction 'MultiCurrency ann) where
  validate RegisterTransaction {..} = foldMap validate registerTransactionPostings

instance (Validity ann, Eq ann) => Validity (RegisterTransaction 'SingleCurrency ann) where
  validate RegisterTransaction {..} = foldMap validate registerTransactionPostings

registerTransactionTotalMulti ::
  (Ord ann) =>
  RegisterTransaction 'MultiCurrency ann ->
  Maybe (Money.MultiAccount (Currency ann))
registerTransactionTotalMulti =
  MultiAccount.sum
    . fmap registerPostingAmountMulti
    . registerTransactionPostings
  where
    registerPostingAmountMulti ::
      RegisterPosting 'MultiCurrency ann ->
      Money.MultiAccount (Currency ann)
    registerPostingAmountMulti RegisterPosting {..} = registerPostingAmount

registerTransactionTotalSingle ::
  RegisterTransaction 'SingleCurrency ann ->
  Money.Account
registerTransactionTotalSingle RegisterTransaction {..} =
  foldl
    (\acc p -> fromMaybe acc (Account.add acc (registerPostingAmount p)))
    Account.zero
    registerTransactionPostings

data RegisterPosting (mode :: AmountMode) ann = RegisterPosting
  { registerPosting :: !(GenLocated ann (Posting ann)),
    -- Amount of this posting
    registerPostingAmount :: !(AmountOf mode ann),
    -- Running total accross blocks
    registerPostingRunningTotal :: !(AmountOf mode ann),
    -- Running total within the block
    registerPostingBlockRunningTotal :: !(AmountOf mode ann)
  }

deriving instance (Show ann, Show (AmountOf mode ann)) => Show (RegisterPosting mode ann)

instance (Validity ann, Eq ann) => Validity (RegisterPosting 'MultiCurrency ann) where
  validate RegisterPosting {..} = validate registerPosting

instance (Validity ann, Eq ann) => Validity (RegisterPosting 'SingleCurrency ann) where
  validate RegisterPosting {..} = validate registerPosting

-- | Wrapper for converted registers, includes target currency for formatting
data ConvertedRegister ann = ConvertedRegister
  { convertedRegisterCurrency :: !(Currency ann),
    convertedRegister :: !(Register 'SingleCurrency ann)
  }

deriving instance (Show ann) => Show (ConvertedRegister ann)

instance (Validity ann, Show ann, Ord ann) => Validity (ConvertedRegister ann) where
  validate ConvertedRegister {..} = validate convertedRegister

-- | Sum type for unified API
data AnyRegister ann
  = AnyMultiCurrency !(Register 'MultiCurrency ann)
  | AnyConverted !(ConvertedRegister ann)

deriving instance (Show ann) => Show (AnyRegister ann)

instance (Validity ann, Show ann, Ord ann) => Validity (AnyRegister ann) where
  validate (AnyMultiCurrency r) = validate r
  validate (AnyConverted r) = validate r

data RegisterError ann
  = RegisterErrorAddError
  | RegisterErrorConvertError !(ConvertError ann)
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterErrorAddError -> undefined
    RegisterErrorConvertError ce -> toReport ce

-- | Produce a register from a ledger, dispatching to either multi-currency
-- or converted mode based on whether a target currency is specified.
produceRegister ::
  forall ann.
  (Ord ann) =>
  -- | Account filter
  Filter ->
  -- | How to group entries into blocks
  BlockSize ->
  -- | Target currency for conversion (Nothing for multi-currency)
  Maybe CurrencySymbol ->
  -- | Whether to include virtual postings
  Bool ->
  -- | Start date filter (inclusive)
  Maybe Day ->
  -- | End date filter (inclusive)
  Maybe Day ->
  -- | Source ledger
  Ledger ann ->
  Validation (RegisterError ann) (AnyRegister ann)
produceRegister f blockSize mCurrencySymbolTo showVirtual mBegin mEnd ledger =
  case mCurrencySymbolTo of
    Nothing -> do
      r <- produceMultiCurrencyRegister f blockSize showVirtual mBegin mEnd ledger
      pure (AnyMultiCurrency r)
    Just currencySymbolTo -> do
      r <- produceConvertedRegister f blockSize currencySymbolTo showVirtual mBegin mEnd ledger
      pure (AnyConverted r)

-- | Produce a multi-currency register (no conversion).
produceMultiCurrencyRegister ::
  forall ann.
  (Ord ann) =>
  -- | Account filter
  Filter ->
  -- | How to group entries into blocks
  BlockSize ->
  -- | Whether to include virtual postings
  Bool ->
  -- | Start date filter (inclusive)
  Maybe Day ->
  -- | End date filter (inclusive)
  Maybe Day ->
  -- | Source ledger
  Ledger ann ->
  Validation (RegisterError ann) (Register 'MultiCurrency ann)
produceMultiCurrencyRegister f blockSize showVirtual mBegin mEnd ledger = do
  flatRegister <- produceFlatRegister f showVirtual mBegin mEnd ledger
  groupMultiIntoBlocks blockSize mBegin mEnd flatRegister

-- | Produce a register with all amounts converted to a single currency.
-- Inserts revaluation entries where price changes affect the running total.
produceConvertedRegister ::
  forall ann.
  (Ord ann) =>
  -- | Account filter
  Filter ->
  -- | How to group entries into blocks
  BlockSize ->
  -- | Target currency to convert all amounts to
  CurrencySymbol ->
  -- | Whether to include virtual postings
  Bool ->
  -- | Start date filter (inclusive)
  Maybe Day ->
  -- | End date filter (inclusive)
  Maybe Day ->
  -- | Source ledger
  Ledger ann ->
  Validation (RegisterError ann) (ConvertedRegister ann)
produceConvertedRegister f blockSize currencySymbolTo showVirtual mBegin mEnd ledger = do
  currencyTo <-
    mapValidationFailure RegisterErrorConvertError $
      lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
  flatRegister <- produceFlatRegister f showVirtual mBegin mEnd ledger
  convertedFlat <- convertFlatRegister currencyTo mEnd (ledgerPrices ledger) flatRegister
  register <- groupSingleIntoBlocks blockSize mBegin mEnd convertedFlat
  pure
    ConvertedRegister
      { convertedRegisterCurrency = currencyTo,
        convertedRegister = register
      }

-- | Produce a flat register from a ledger (Stage 1).
--
-- Each entry contains the price graph at that point in time, which is used
-- for later conversion to a single currency.
produceFlatRegister ::
  forall ann.
  (Ord ann) =>
  -- | Account filter
  Filter ->
  -- | Whether to include virtual postings
  Bool ->
  -- | Start date filter (inclusive)
  Maybe Day ->
  -- | End date filter (inclusive)
  Maybe Day ->
  -- | Source ledger
  Ledger ann ->
  Validation (RegisterError ann) (FlatRegister ann)
produceFlatRegister f showVirtual mBegin mEnd ledger = do
  let prices = pricesToDailyPriceGraphs (ledgerPrices ledger)

  let go ::
        Money.MultiAccount (Currency ann) ->
        [GenLocated ann (Transaction ann)] ->
        Validation (RegisterError ann) [FlatEntry ann]
      go _ [] = pure []
      go runningTotal (ltx : rest) = do
        let Located _ tx = ltx
            Located _ ts = transactionTimestamp tx
            day = Timestamp.toDay ts
            dayPrices = case M.lookupLE day prices of
              Nothing -> MemoisedPriceGraph.empty
              Just (_, pg) -> pg
        mEntry <- processFlatTransaction f showVirtual mBegin mEnd dayPrices runningTotal ltx
        case mEntry of
          Nothing -> go runningTotal rest
          Just entry -> do
            let newRunningTotal = flatEntryRunningTotal entry
            entries <- go newRunningTotal rest
            pure (entry : entries)

  entries <- go MultiAccount.zero (V.toList (ledgerTransactions ledger))
  let total = case entries of
        [] -> MultiAccount.zero
        _ -> flatEntryRunningTotal (last entries)
  pure
    FlatRegister
      { flatRegisterEntries = entries,
        flatRegisterTotal = total
      }

processFlatTransaction ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  MemoisedPriceGraph (Currency ann) ->
  Money.MultiAccount (Currency ann) ->
  GenLocated ann (Transaction ann) ->
  Validation (RegisterError ann) (Maybe (FlatEntry ann))
processFlatTransaction f showVirtual mBegin mEnd priceGraph runningTotal (Located _ Transaction {..}) =
  if timestampPassesDayFilter mBegin mEnd transactionTimestamp
    then do
      let goPostings running = \case
            [] -> pure ([], running)
            posting : rest -> do
              mPosting <- processFlatPosting f showVirtual running posting
              case mPosting of
                Nothing -> goPostings running rest
                Just flatPosting -> do
                  (ps, finalRunning) <- goPostings (flatPostingRunningTotal flatPosting) rest
                  pure (flatPosting : ps, finalRunning)
      (postings, newRunningTotal) <- goPostings runningTotal (V.toList transactionPostings)
      pure $
        if null postings
          then Nothing
          else
            Just
              FlatEntry
                { flatEntryTimestamp = transactionTimestamp,
                  flatEntryDescription = transactionDescription,
                  flatEntryPostings = V.fromList postings,
                  flatEntryPriceGraph = priceGraph,
                  flatEntryRunningTotal = newRunningTotal
                }
    else pure Nothing

processFlatPosting ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Money.MultiAccount (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation (RegisterError ann) (Maybe (FlatPosting ann))
processFlatPosting f showVirtual runningTotal lp@(Located _ Posting {..}) = do
  let Located _ an = postingAccountName
  let includedByFilter = Filter.predicate f an
  if (postingReal || (not postingReal && showVirtual)) && includedByFilter
    then do
      let Located _ currency = postingCurrency
          Located _ account = postingAccount
          amount = MultiAccount.fromAccount currency account
      case MultiAccount.add runningTotal amount of
        Nothing -> validationFailure RegisterErrorAddError
        Just newRunning ->
          pure $
            Just
              FlatPosting
                { flatPosting = lp,
                  flatPostingAmount = amount,
                  flatPostingRunningTotal = newRunning
                }
    else pure Nothing

-- | Convert a flat register to a single currency (Stage 2).
--
-- This inserts revaluation entries where price changes affect the running total.
convertFlatRegister ::
  forall ann.
  (Ord ann) =>
  -- | Target currency to convert all amounts to
  Currency ann ->
  -- | End date filter (inclusive)
  Maybe Day ->
  -- | Price declarations from the ledger
  Vector (GenLocated ann (Price ann)) ->
  -- | Flat register to convert
  FlatRegister ann ->
  Validation (RegisterError ann) (ConvertedFlatRegister ann)
convertFlatRegister currencyTo mEnd pricesVec FlatRegister {..} = do
  let prices = pricesToDailyPriceGraphs pricesVec
      lastPriceDay = case M.lookupMax prices of
        Nothing -> Nothing
        Just (day, _) -> Just day
      -- Limit trailing revaluations to the end date filter
      effectiveLastPriceDay = case (lastPriceDay, mEnd) of
        (Just lpd, Just end) -> Just (min lpd end)
        (lpd, Nothing) -> lpd
        (Nothing, _) -> Nothing

  let go ::
        Money.Account ->
        Money.MultiAccount (Currency ann) ->
        Maybe Day ->
        [FlatEntry ann] ->
        Validation (RegisterError ann) [ConvertedFlatEntry ann]
      go convertedRunning rawBalances mLastDay [] =
        case (mLastDay, effectiveLastPriceDay) of
          (Just lastEntryDay, Just lastPDay)
            | lastEntryDay < lastPDay ->
                map ConvertedFlatEntryRevaluation
                  <$> computeRevaluations lastEntryDay lastPDay prices pricesVec currencyTo rawBalances convertedRunning
          _ -> pure []
      go convertedRunning rawBalances mLastDay (entry : rest) = do
        let Located _ ts = flatEntryTimestamp entry
            day = Timestamp.toDay ts
            currentPrices = flatEntryPriceGraph entry

        revalEntries <- case mLastDay of
          Nothing -> pure []
          Just lastDay ->
            if lastDay < day
              then computeRevaluations lastDay day prices pricesVec currencyTo rawBalances convertedRunning
              else pure []

        let convertedAfterRevals = case revalEntries of
              [] -> convertedRunning
              _ -> convertedFlatRevaluationRunningTotal (last revalEntries)

        (convertedEntry, newConvertedRunning, newRawBalances) <-
          convertFlatEntry currencyTo currentPrices convertedAfterRevals rawBalances entry

        restEntries <- go newConvertedRunning newRawBalances (Just day) rest
        pure $ map ConvertedFlatEntryRevaluation revalEntries ++ [ConvertedFlatEntryTransaction convertedEntry] ++ restEntries

  entries <- go Account.zero MultiAccount.zero Nothing flatRegisterEntries
  let total = case entries of
        [] -> Account.zero
        _ -> convertedFlatEntryRunningTotal (last entries)
  pure
    ConvertedFlatRegister
      { convertedFlatRegisterCurrency = currencyTo,
        convertedFlatRegisterEntries = entries,
        convertedFlatRegisterTotal = total
      }

convertedFlatEntryRunningTotal ::
  ConvertedFlatEntry ann ->
  Money.Account
convertedFlatEntryRunningTotal = \case
  ConvertedFlatEntryTransaction t -> convertedFlatTransactionRunningTotal t
  ConvertedFlatEntryRevaluation r -> convertedFlatRevaluationRunningTotal r

convertFlatEntry ::
  forall ann.
  (Ord ann) =>
  Currency ann ->
  MemoisedPriceGraph (Currency ann) ->
  Money.Account ->
  Money.MultiAccount (Currency ann) ->
  FlatEntry ann ->
  Validation
    (RegisterError ann)
    (ConvertedFlatTransaction ann, Money.Account, Money.MultiAccount (Currency ann))
convertFlatEntry currencyTo priceGraph convertedRunning rawBalances FlatEntry {..} = do
  let goPostings ::
        Money.Account ->
        Money.MultiAccount (Currency ann) ->
        [FlatPosting ann] ->
        Validation
          (RegisterError ann)
          ([ConvertedFlatPosting ann], Money.Account, Money.MultiAccount (Currency ann))
      goPostings running rawBal [] = pure ([], running, rawBal)
      goPostings running rawBal (fp : fps) = do
        (converted, newRunning, newRawBal) <- convertFlatPosting currencyTo priceGraph running rawBal fp
        (rest, finalRunning, finalRawBal) <- goPostings newRunning newRawBal fps
        pure (converted : rest, finalRunning, finalRawBal)

  (postings, newConvertedRunning, newRawBalances) <-
    goPostings convertedRunning rawBalances (V.toList flatEntryPostings)

  pure
    ( ConvertedFlatTransaction
        { convertedFlatTransactionTimestamp = flatEntryTimestamp,
          convertedFlatTransactionDescription = flatEntryDescription,
          convertedFlatTransactionPostings = V.fromList postings,
          convertedFlatTransactionRunningTotal = newConvertedRunning
        },
      newConvertedRunning,
      newRawBalances
    )

convertFlatPosting ::
  (Ord ann) =>
  Currency ann ->
  MemoisedPriceGraph (Currency ann) ->
  Money.Account ->
  Money.MultiAccount (Currency ann) ->
  FlatPosting ann ->
  Validation
    (RegisterError ann)
    (ConvertedFlatPosting ann, Money.Account, Money.MultiAccount (Currency ann))
convertFlatPosting currencyTo priceGraph convertedRunning rawBalances FlatPosting {..} = do
  let Located pl Posting {..} = flatPosting
      Located al _ = postingAccount

  -- Convert the amount
  converted <-
    mapValidationFailure RegisterErrorConvertError $
      convertMultiAccountToAccount (Just al) priceGraph currencyTo flatPostingAmount

  -- Update running totals
  newConvertedRunning <- case Account.add convertedRunning converted of
    Nothing -> validationFailure RegisterErrorAddError
    Just r -> pure r

  newRawBalances <- case MultiAccount.add rawBalances flatPostingAmount of
    Nothing -> validationFailure RegisterErrorAddError
    Just r -> pure r

  -- Create converted posting with converted currency/account
  let convertedPosting =
        Located
          pl
          Posting
            { postingAccountName = postingAccountName,
              postingCurrency = fmap (const currencyTo) postingCurrency,
              postingAccount = Located al converted,
              postingReal = postingReal,
              postingCost = postingCost,
              postingAmountRatio = postingAmountRatio
            }

  pure
    ( ConvertedFlatPosting
        { convertedFlatPosting = convertedPosting,
          convertedFlatPostingAmount = converted,
          convertedFlatPostingRunningTotal = newConvertedRunning
        },
      newConvertedRunning,
      newRawBalances
    )

computeRevaluations ::
  forall ann.
  (Ord ann) =>
  Day ->
  Day ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Vector (GenLocated ann (Price ann)) ->
  Currency ann ->
  Money.MultiAccount (Currency ann) ->
  Money.Account ->
  Validation (RegisterError ann) [ConvertedFlatRevaluation ann]
computeRevaluations lastDay currentDay priceGraphs allPrices currencyTo rawBalances initialConvertedRunning = do
  let relevantPrices =
        V.toList $
          V.filter
            ( \(Located _ Price {..}) ->
                let Located _ ts = priceTimestamp
                    day = Timestamp.toDay ts
                 in day > lastDay && day <= currentDay
            )
            allPrices

  goRevals initialConvertedRunning relevantPrices
  where
    goRevals ::
      Money.Account ->
      [GenLocated ann (Price ann)] ->
      Validation (RegisterError ann) [ConvertedFlatRevaluation ann]
    goRevals _ [] = pure []
    goRevals running (Located _ Price {..} : rest) = do
      let Located _ ts = priceTimestamp
          priceDay = Timestamp.toDay ts
          oldPG = case M.lookupLT priceDay priceGraphs of
            Nothing -> MemoisedPriceGraph.empty
            Just (_, pg) -> pg
          newPG = case M.lookupLE priceDay priceGraphs of
            Nothing -> MemoisedPriceGraph.empty
            Just (_, pg) -> pg

      let Located _ fromCur = priceCurrency
          Located _ Cost {..} = priceCost
          Located _ toCur = costCurrency
          Money.MultiAccount balanceMap = rawBalances
          affectedCurrencies = [fromCur, toCur]
          affectedBalances =
            [ (cur, bal)
            | cur <- affectedCurrencies,
              Just bal <- [M.lookup cur balanceMap],
              bal /= Account.zero,
              cur /= currencyTo
            ]

      case affectedBalances of
        [] -> goRevals running rest
        _ -> do
          let computeForCurrency (cur, bal) = do
                let singleBalance = MultiAccount.fromAccount cur bal
                let oldConvertedResult =
                      mapValidationFailure RegisterErrorConvertError $
                        convertMultiAccountToAccount Nothing oldPG currencyTo singleBalance
                    oldConverted = case oldConvertedResult of
                      Failure _ -> Account.zero
                      Success v -> v
                newConverted <-
                  mapValidationFailure RegisterErrorConvertError $
                    convertMultiAccountToAccount Nothing newPG currencyTo singleBalance
                case Account.subtract newConverted oldConverted of
                  Nothing -> validationFailure RegisterErrorAddError
                  Just amt -> pure amt

          revalAmounts <- traverse computeForCurrency affectedBalances
          let totalRevalAmount = foldl (\acc a -> fromMaybe acc (Account.add acc a)) Account.zero revalAmounts

          if totalRevalAmount == Account.zero
            then goRevals running rest
            else do
              newRunning <- case Account.add running totalRevalAmount of
                Nothing -> validationFailure RegisterErrorAddError
                Just r -> pure r
              let reval =
                    ConvertedFlatRevaluation
                      { convertedFlatRevaluationTimestamp = priceTimestamp,
                        convertedFlatRevaluationCurrency = priceCurrency,
                        convertedFlatRevaluationAmount = totalRevalAmount,
                        convertedFlatRevaluationRunningTotal = newRunning
                      }
              restRevals <- goRevals newRunning rest
              pure (reval : restRevals)

-- | Group a flat register into blocks (Stage 3, multi-currency).
--
-- Groups entries into time periods and computes running totals and averages.
groupMultiIntoBlocks ::
  forall ann.
  (Ord ann) =>
  -- | How to group entries into blocks
  BlockSize ->
  -- | Start date for empty block generation
  Maybe Day ->
  -- | End date for empty block generation
  Maybe Day ->
  -- | Flat register to group
  FlatRegister ann ->
  Validation (RegisterError ann) (Register 'MultiCurrency ann)
groupMultiIntoBlocks blockSize mBegin mEnd FlatRegister {..} = do
  let mBeginBlock = (\begin -> dayBlockTitle begin blockSize) <$> mBegin
      mEndBlock = (\end -> dayBlockTitle end blockSize) <$> mEnd

  let entryBlockTitle ::
        FlatEntry ann ->
        Block
      entryBlockTitle entry =
        let Located _ ts = flatEntryTimestamp entry
         in timestampBlockTitle ts blockSize

  let convertEntry ::
        Money.MultiAccount (Currency ann) ->
        FlatEntry ann ->
        (RegisterEntry 'MultiCurrency ann, Money.MultiAccount (Currency ann))
      convertEntry blockRunning entry =
        let (postings, finalBlockRunning) = convertPostings blockRunning (V.toList (flatEntryPostings entry))
            regTx =
              RegisterTransaction
                { registerTransactionTimestamp = flatEntryTimestamp entry,
                  registerTransactionDescription = flatEntryDescription entry,
                  registerTransactionPostings = V.fromList postings
                }
         in (RegisterEntryTransaction regTx, finalBlockRunning)

      convertPostings ::
        Money.MultiAccount (Currency ann) ->
        [FlatPosting ann] ->
        ([RegisterPosting 'MultiCurrency ann], Money.MultiAccount (Currency ann))
      convertPostings blockRunning [] = ([], blockRunning)
      convertPostings blockRunning (fp : fps) =
        let newBlockRunning = fromMaybe blockRunning (MultiAccount.add blockRunning (flatPostingAmount fp))
            regPosting =
              RegisterPosting
                { registerPosting = flatPosting fp,
                  registerPostingAmount = flatPostingAmount fp,
                  registerPostingRunningTotal = flatPostingRunningTotal fp,
                  registerPostingBlockRunningTotal = newBlockRunning
                }
            (rest, finalBlockRunning) = convertPostings newBlockRunning fps
         in (regPosting : rest, finalBlockRunning)

  let goBlocks ::
        Maybe Block ->
        Integer ->
        Money.MultiAccount (Currency ann) ->
        [FlatEntry ann] ->
        [RegisterBlock 'MultiCurrency ann]
      goBlocks mCurrentBlock blockNum runningTotal entries =
        case findNextBlock mCurrentBlock entries of
          Nothing ->
            case (mCurrentBlock, mEndBlock) of
              (Just current, Just endBlock)
                | current <= endBlock ->
                    let emptyBlock = makeEmptyBlock current runningTotal blockNum
                     in emptyBlock : goBlocks (Just (nextBlock current)) blockNum runningTotal []
              _ -> []
          Just (blockTitle, blockEntries, restEntries) ->
            let emptyBlocks = case (mCurrentBlock, mEndBlock) of
                  (Just current, Just _)
                    | current < blockTitle ->
                        generateEmptyBlocks current blockTitle runningTotal blockNum
                  _ -> []
                numEmpty = toInteger (length emptyBlocks)
                (convertedEntries, blockTotal) = convertEntriesForBlock blockEntries
                newRunningTotal = case blockEntries of
                  [] -> runningTotal
                  _ -> flatEntryRunningTotal (last blockEntries)
                effectiveBlockNum = blockNum + numEmpty
                nextBlockNum = effectiveBlockNum + 1
                block =
                  RegisterBlock
                    { registerBlockTitle = blockTitle,
                      registerBlockEntries = V.fromList convertedEntries,
                      registerBlockTotal = blockTotal,
                      registerBlockRunningTotal = newRunningTotal,
                      registerBlockRunningAverage = computeMultiAverage newRunningTotal effectiveBlockNum
                    }
                shouldContinue = case mEndBlock of
                  Nothing -> not (null restEntries)
                  Just endBlock -> blockTitle < endBlock
             in if shouldContinue
                  then emptyBlocks ++ [block] ++ goBlocks (Just (nextBlock blockTitle)) nextBlockNum newRunningTotal restEntries
                  else emptyBlocks ++ [block]

      findNextBlock ::
        Maybe Block ->
        [FlatEntry ann] ->
        Maybe (Block, [FlatEntry ann], [FlatEntry ann])
      findNextBlock mCurrentBlock entries = case entries of
        [] -> Nothing
        (e : _) ->
          let blockTitle = entryBlockTitle e
              shouldProcess = case mCurrentBlock of
                Nothing -> True
                Just current -> blockTitle >= current
           in if shouldProcess
                then
                  let (blockEntries, rest) = span (\entry -> entryBlockTitle entry == blockTitle) entries
                   in Just (blockTitle, blockEntries, rest)
                else findNextBlock mCurrentBlock (dropWhile (\entry -> entryBlockTitle entry < fromMaybe blockTitle mCurrentBlock) entries)

      generateEmptyBlocks ::
        Block ->
        Block ->
        Money.MultiAccount (Currency ann) ->
        Integer ->
        [RegisterBlock 'MultiCurrency ann]
      generateEmptyBlocks current target runningTotal blockNum
        | current >= target = []
        | otherwise =
            let emptyBlock = makeEmptyBlock current runningTotal blockNum
             in emptyBlock : generateEmptyBlocks (nextBlock current) target runningTotal blockNum

      makeEmptyBlock ::
        Block ->
        Money.MultiAccount (Currency ann) ->
        Integer ->
        RegisterBlock 'MultiCurrency ann
      makeEmptyBlock title runningTotal blockNum =
        RegisterBlock
          { registerBlockTitle = title,
            registerBlockEntries = V.empty,
            registerBlockTotal = MultiAccount.zero,
            registerBlockRunningTotal = runningTotal,
            registerBlockRunningAverage = computeMultiAverage runningTotal blockNum
          }

      convertEntriesForBlock ::
        [FlatEntry ann] ->
        ([RegisterEntry 'MultiCurrency ann], Money.MultiAccount (Currency ann))
      convertEntriesForBlock entries =
        let go _ [] = ([], MultiAccount.zero)
            go blockRunning (e : es) =
              let (converted, newBlockRunning) = convertEntry blockRunning e
                  (rest, restTotal) = go newBlockRunning es
                  entryTotal = fromMaybe MultiAccount.zero (registerEntryTotalMulti converted)
                  combinedTotal = fromMaybe entryTotal (MultiAccount.add entryTotal restTotal)
               in (converted : rest, combinedTotal)
         in go MultiAccount.zero entries

  let blocks = goBlocks mBeginBlock 1 MultiAccount.zero flatRegisterEntries
  pure
    Register
      { registerBlockSize = blockSize,
        registerBlocks = V.fromList blocks,
        registerTotal = flatRegisterTotal
      }

computeMultiAverage ::
  Money.MultiAccount (Currency ann) ->
  Integer ->
  Money.MultiAccount (Currency ann)
computeMultiAverage (Money.MultiAccount m) blockNum =
  Money.MultiAccount $ M.map go m
  where
    go a =
      let (ma, _) = Account.fraction Money.RoundDown a (1 % blockNum)
       in fromMaybe Account.zero ma

-- | Group a converted flat register into blocks (Stage 3, single-currency).
--
-- Groups entries into time periods and computes running totals and averages.
groupSingleIntoBlocks ::
  forall ann.
  -- | How to group entries into blocks
  BlockSize ->
  -- | Start date for empty block generation
  Maybe Day ->
  -- | End date for empty block generation
  Maybe Day ->
  -- | Converted flat register to group
  ConvertedFlatRegister ann ->
  Validation (RegisterError ann) (Register 'SingleCurrency ann)
groupSingleIntoBlocks blockSize mBegin mEnd ConvertedFlatRegister {..} = do
  let mBeginBlock = (\begin -> dayBlockTitle begin blockSize) <$> mBegin
      mEndBlock = (\end -> dayBlockTitle end blockSize) <$> mEnd

  let entryTimestamp ::
        ConvertedFlatEntry ann ->
        GenLocated ann Timestamp
      entryTimestamp = \case
        ConvertedFlatEntryTransaction t -> convertedFlatTransactionTimestamp t
        ConvertedFlatEntryRevaluation r -> convertedFlatRevaluationTimestamp r

      entryBlockTitle ::
        ConvertedFlatEntry ann ->
        Block
      entryBlockTitle entry =
        let Located _ ts = entryTimestamp entry
         in timestampBlockTitle ts blockSize

  let convertEntry ::
        Money.Account ->
        ConvertedFlatEntry ann ->
        (RegisterEntry 'SingleCurrency ann, Money.Account)
      convertEntry blockRunning = \case
        ConvertedFlatEntryTransaction t ->
          let (postings, finalBlockRunning) = convertPostings blockRunning (V.toList (convertedFlatTransactionPostings t))
              regTx =
                RegisterTransaction
                  { registerTransactionTimestamp = convertedFlatTransactionTimestamp t,
                    registerTransactionDescription = convertedFlatTransactionDescription t,
                    registerTransactionPostings = V.fromList postings
                  }
           in (RegisterEntryTransaction regTx, finalBlockRunning)
        ConvertedFlatEntryRevaluation r ->
          case Account.add blockRunning (convertedFlatRevaluationAmount r) of
            Nothing -> (RegisterEntryRevaluation (convertReval blockRunning r), blockRunning)
            Just newBlockRunning ->
              (RegisterEntryRevaluation (convertReval newBlockRunning r), newBlockRunning)

      convertReval ::
        Money.Account ->
        ConvertedFlatRevaluation ann ->
        RegisterRevaluation ann
      convertReval blockRunning r =
        RegisterRevaluation
          { registerRevaluationTimestamp = convertedFlatRevaluationTimestamp r,
            registerRevaluationCurrency = convertedFlatRevaluationCurrency r,
            registerRevaluationAmount = convertedFlatRevaluationAmount r,
            registerRevaluationRunningTotal = convertedFlatRevaluationRunningTotal r,
            registerRevaluationBlockRunningTotal = blockRunning
          }

      convertPostings ::
        Money.Account ->
        [ConvertedFlatPosting ann] ->
        ([RegisterPosting 'SingleCurrency ann], Money.Account)
      convertPostings blockRunning [] = ([], blockRunning)
      convertPostings blockRunning (cp : cps) =
        let newBlockRunning = fromMaybe blockRunning (Account.add blockRunning (convertedFlatPostingAmount cp))
            regPosting =
              RegisterPosting
                { registerPosting = convertedFlatPosting cp,
                  registerPostingAmount = convertedFlatPostingAmount cp,
                  registerPostingRunningTotal = convertedFlatPostingRunningTotal cp,
                  registerPostingBlockRunningTotal = newBlockRunning
                }
            (rest, finalBlockRunning) = convertPostings newBlockRunning cps
         in (regPosting : rest, finalBlockRunning)

  let goBlocks ::
        Maybe Block ->
        Integer ->
        Money.Account ->
        [ConvertedFlatEntry ann] ->
        [RegisterBlock 'SingleCurrency ann]
      goBlocks mCurrentBlock blockNum runningTotal entries =
        case findNextBlock mCurrentBlock entries of
          Nothing ->
            case (mCurrentBlock, mEndBlock) of
              (Just current, Just endBlock)
                | current <= endBlock ->
                    let emptyBlock = makeEmptyBlock current runningTotal blockNum
                     in emptyBlock : goBlocks (Just (nextBlock current)) blockNum runningTotal []
              _ -> []
          Just (blockTitle, blockEntries, restEntries) ->
            let emptyBlocks = case (mCurrentBlock, mEndBlock) of
                  (Just current, Just _)
                    | current < blockTitle ->
                        generateEmptyBlocks current blockTitle runningTotal blockNum
                  _ -> []
                numEmpty = toInteger (length emptyBlocks)
                (convertedEntries, blockTotal) = convertEntriesForBlock blockEntries
                newRunningTotal = case blockEntries of
                  [] -> runningTotal
                  _ -> convertedFlatEntryRunningTotal (last blockEntries)
                hasTransactions = any isTransactionEntry blockEntries
                effectiveBlockNum = blockNum + numEmpty
                nextBlockNum = if hasTransactions then effectiveBlockNum + 1 else effectiveBlockNum
                block =
                  RegisterBlock
                    { registerBlockTitle = blockTitle,
                      registerBlockEntries = V.fromList convertedEntries,
                      registerBlockTotal = blockTotal,
                      registerBlockRunningTotal = newRunningTotal,
                      registerBlockRunningAverage =
                        computeSingleAverage newRunningTotal (if hasTransactions then effectiveBlockNum else max 1 (effectiveBlockNum - 1))
                    }
                shouldContinue = case mEndBlock of
                  Nothing -> not (null restEntries)
                  Just endBlock -> blockTitle < endBlock
             in if shouldContinue
                  then emptyBlocks ++ [block] ++ goBlocks (Just (nextBlock blockTitle)) nextBlockNum newRunningTotal restEntries
                  else emptyBlocks ++ [block]

      findNextBlock ::
        Maybe Block ->
        [ConvertedFlatEntry ann] ->
        Maybe (Block, [ConvertedFlatEntry ann], [ConvertedFlatEntry ann])
      findNextBlock mCurrentBlock entries = case entries of
        [] -> Nothing
        (e : _) ->
          let blockTitle = entryBlockTitle e
              shouldProcess = case mCurrentBlock of
                Nothing -> True
                Just current -> blockTitle >= current
           in if shouldProcess
                then
                  let (blockEntries, rest) = span (\entry -> entryBlockTitle entry == blockTitle) entries
                   in Just (blockTitle, blockEntries, rest)
                else findNextBlock mCurrentBlock (dropWhile (\entry -> entryBlockTitle entry < fromMaybe blockTitle mCurrentBlock) entries)

      generateEmptyBlocks ::
        Block ->
        Block ->
        Money.Account ->
        Integer ->
        [RegisterBlock 'SingleCurrency ann]
      generateEmptyBlocks current target runningTotal blockNum
        | current >= target = []
        | otherwise =
            let emptyBlock = makeEmptyBlock current runningTotal blockNum
             in emptyBlock : generateEmptyBlocks (nextBlock current) target runningTotal blockNum

      makeEmptyBlock ::
        Block ->
        Money.Account ->
        Integer ->
        RegisterBlock 'SingleCurrency ann
      makeEmptyBlock title runningTotal blockNum =
        RegisterBlock
          { registerBlockTitle = title,
            registerBlockEntries = V.empty,
            registerBlockTotal = Account.zero,
            registerBlockRunningTotal = runningTotal,
            registerBlockRunningAverage = computeSingleAverage runningTotal blockNum
          }

      convertEntriesForBlock ::
        [ConvertedFlatEntry ann] ->
        ([RegisterEntry 'SingleCurrency ann], Money.Account)
      convertEntriesForBlock entries =
        let go _ [] = ([], Account.zero)
            go blockRunning (e : es) =
              let (converted, newBlockRunning) = convertEntry blockRunning e
                  (rest, restTotal) = go newBlockRunning es
                  entryTotal = registerEntryTotalSingle converted
                  combinedTotal = fromMaybe entryTotal (Account.add entryTotal restTotal)
               in (converted : rest, combinedTotal)
         in go Account.zero entries

      isTransactionEntry ::
        ConvertedFlatEntry ann ->
        Bool
      isTransactionEntry = \case
        ConvertedFlatEntryTransaction _ -> True
        ConvertedFlatEntryRevaluation _ -> False

  let blocks = goBlocks mBeginBlock 1 Account.zero convertedFlatRegisterEntries
  pure
    Register
      { registerBlockSize = blockSize,
        registerBlocks = V.fromList blocks,
        registerTotal = convertedFlatRegisterTotal
      }

computeSingleAverage ::
  Money.Account ->
  Integer ->
  Money.Account
computeSingleAverage acc blockNum =
  let (ma, _) = Account.fraction Money.RoundDown acc (1 % blockNum)
   in fromMaybe Account.zero ma

-- Helpers

dayPassesDayFilter ::
  Maybe Day ->
  Maybe Day ->
  Day ->
  Bool
dayPassesDayFilter mBegin mEnd day =
  and
    [ case mBegin of
        Nothing -> True
        Just begin -> day >= begin,
      case mEnd of
        Nothing -> True
        Just end -> day <= end
    ]

timestampPassesDayFilter ::
  Maybe Day ->
  Maybe Day ->
  GenLocated ann Timestamp ->
  Bool
timestampPassesDayFilter mBegin mEnd (Located _ ts) =
  and
    [ case mBegin of
        Nothing -> True
        Just begin -> Timestamp.toDay ts >= begin,
      case mEnd of
        Nothing -> True
        Just end -> Timestamp.toDay ts <= end
    ]
