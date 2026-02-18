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
import Centjes.Report.EvaluatedLedger
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation (ToReport (..), Validation (..), mapValidationFailure, validationFailure)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Validity (Validity (..), declare)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
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

-- | A flat posting with its amount and running total.
data FlatPosting ann = FlatPosting
  { flatPosting :: !(GenLocated ann (Posting ann)),
    flatPostingAmount :: !(Money.MultiAccount (Currency ann)),
    flatPostingRunningTotal :: !(Money.MultiAccount (Currency ann))
  }

-- Converted flat register types (Stage 2 output)

-- | A converted flat register: entries converted to a single currency.
-- May include revaluation entries where prices changed.
data ConvertedFlatRegister ann = ConvertedFlatRegister
  { convertedFlatRegisterCurrency :: !(Currency ann),
    convertedFlatRegisterEntries :: ![ConvertedFlatEntry ann],
    convertedFlatRegisterTotal :: !Money.Account
  }

-- | A converted flat entry: either a transaction or a revaluation.
data ConvertedFlatEntry ann
  = ConvertedFlatEntryTransaction !(ConvertedFlatTransaction ann)
  | ConvertedFlatEntryRevaluation !(ConvertedFlatRevaluation ann)

-- | A converted transaction with single-currency amounts.
data ConvertedFlatTransaction ann = ConvertedFlatTransaction
  { convertedFlatTransactionTimestamp :: !(GenLocated ann Timestamp),
    convertedFlatTransactionDescription :: !(Maybe (GenLocated ann Description)),
    convertedFlatTransactionPostings :: !(Vector (ConvertedFlatPosting ann)),
    convertedFlatTransactionRunningTotal :: !Money.Account
  }

-- | A converted posting with single-currency amount.
data ConvertedFlatPosting ann = ConvertedFlatPosting
  { convertedFlatPosting :: !(GenLocated ann (Posting ann)),
    convertedFlatPostingAmount :: !Money.Account,
    convertedFlatPostingRunningTotal :: !Money.Account
  }

-- | A revaluation entry from price changes.
data ConvertedFlatRevaluation ann = ConvertedFlatRevaluation
  { convertedFlatRevaluationTimestamp :: !(GenLocated ann Timestamp),
    -- | The currencies whose prices changed (may be multiple for same-day prices)
    convertedFlatRevaluationCurrencies :: !(NonEmpty (GenLocated ann (Currency ann))),
    convertedFlatRevaluationAmount :: !Money.Account,
    convertedFlatRevaluationRunningTotal :: !Money.Account
  }

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
        foldMap validate registerBlockEntries
        -- Note: We don't validate that blockTotal == sum of entries because
        -- entries use individually-rounded amounts while blockTotal uses
        -- bulk-converted running total differences for consistency with balance.
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

-- | A revaluation entry representing gain/loss due to price changes
-- Always uses single-currency amounts
data RegisterRevaluation ann = RegisterRevaluation
  { registerRevaluationTimestamp :: !(GenLocated ann Timestamp),
    -- | The currencies whose prices changed (may be multiple for same-day prices)
    registerRevaluationCurrencies :: !(NonEmpty (GenLocated ann (Currency ann))),
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
  | RegisterErrorEvaluatedLedger !(EvaluatedLedgerError ann)
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    -- [tag:RE_ADD_ERROR] At least one test per error: test_resources/register/error/RE_ADD_ERROR.cent
    RegisterErrorAddError ->
      Err
        (Just "RE_ADD_ERROR")
        "Could not add amounts together because the result got too big."
        []
        []
    -- [tag:RE_CONVERT_ERROR] At least one test per error: test_resources/register/error/CONVERT_ERROR_*.cent
    RegisterErrorConvertError convertError -> toReport convertError
    RegisterErrorEvaluatedLedger evaluatedLedgerError -> toReport evaluatedLedgerError

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
produceRegister f blockSize mCurrencySymbolTo showVirtual mBegin mEnd ledger = do
  evaluatedLedger <- mapValidationFailure RegisterErrorEvaluatedLedger $ produceEvaluatedLedger ledger
  case mCurrencySymbolTo of
    Nothing -> do
      r <- produceMultiCurrencyRegister f blockSize showVirtual mBegin mEnd evaluatedLedger
      pure (AnyMultiCurrency r)
    Just currencySymbolTo -> do
      r <- produceConvertedRegister f blockSize currencySymbolTo showVirtual mBegin mEnd evaluatedLedger
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
  -- | Evaluated ledger
  EvaluatedLedger ann ->
  Validation (RegisterError ann) (Register 'MultiCurrency ann)
produceMultiCurrencyRegister f blockSize showVirtual mBegin mEnd evaluatedLedger = do
  flatRegister <- produceFlatRegister f showVirtual mBegin mEnd evaluatedLedger
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
  -- | Evaluated ledger
  EvaluatedLedger ann ->
  Validation (RegisterError ann) (ConvertedRegister ann)
produceConvertedRegister f blockSize currencySymbolTo showVirtual mBegin mEnd evaluatedLedger = do
  let ledger = evaluatedLedgerSource evaluatedLedger
  currencyTo <-
    mapValidationFailure RegisterErrorConvertError $
      lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
  flatRegister <- produceFlatRegister f showVirtual mBegin mEnd evaluatedLedger
  let dailyPriceGraphs = buildDailyPriceGraphsFromEntries (evaluatedLedgerEntries evaluatedLedger)
  convertedFlat <- convertFlatRegister currencyTo mEnd dailyPriceGraphs (ledgerPrices ledger) flatRegister
  register <- groupSingleIntoBlocks blockSize mBegin mEnd convertedFlat
  pure
    ConvertedRegister
      { convertedRegisterCurrency = currencyTo,
        convertedRegister = register
      }

-- | Produce a flat register from an evaluated ledger (Stage 1).
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
  -- | Evaluated ledger
  EvaluatedLedger ann ->
  Validation (RegisterError ann) (FlatRegister ann)
produceFlatRegister f showVirtual mBegin mEnd evaluatedLedger = do
  let go ::
        Money.MultiAccount (Currency ann) ->
        [EvaluatedEntry ann] ->
        Validation (RegisterError ann) [FlatEntry ann]
      go _ [] = pure []
      go runningTotal (entry : rest) = case entry of
        EvaluatedEntryPrice _ -> go runningTotal rest
        EvaluatedEntryTransaction evaluatedTransaction -> do
          mFlatEntry <- processFlatTransaction f showVirtual mBegin mEnd runningTotal evaluatedTransaction
          case mFlatEntry of
            Nothing -> go runningTotal rest
            Just flatEntry -> do
              let newRunningTotal = flatEntryRunningTotal flatEntry
              entries <- go newRunningTotal rest
              pure (flatEntry : entries)

  entries <- go MultiAccount.zero (V.toList (evaluatedLedgerEntries evaluatedLedger))
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
  Money.MultiAccount (Currency ann) ->
  EvaluatedTransaction ann ->
  Validation (RegisterError ann) (Maybe (FlatEntry ann))
processFlatTransaction f showVirtual mBegin mEnd runningTotal evaluatedTransaction =
  let Located _ Transaction {..} = evaluatedTransactionLocated evaluatedTransaction
      priceGraph = evaluatedTransactionPriceGraph evaluatedTransaction
   in if timestampPassesDayFilter mBegin mEnd transactionTimestamp
        then do
          let goPostings running = \case
                [] -> pure ([], running)
                evaluatedPosting : rest -> do
                  mPosting <- processFlatPosting f showVirtual running evaluatedPosting
                  case mPosting of
                    Nothing -> goPostings running rest
                    Just flatPosting -> do
                      (ps, finalRunning) <- goPostings (flatPostingRunningTotal flatPosting) rest
                      pure (flatPosting : ps, finalRunning)
          (postings, newRunningTotal) <- goPostings runningTotal (V.toList (evaluatedTransactionPostings evaluatedTransaction))
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
  EvaluatedPosting ann ->
  Validation (RegisterError ann) (Maybe (FlatPosting ann))
processFlatPosting f showVirtual runningTotal evaluatedPosting = do
  let lp@(Located _ Posting {..}) = evaluatedPostingLocated evaluatedPosting
  let Located _ an = postingAccountName
  let includedByFilter = Filter.predicate f an
  if (postingReal || (not postingReal && showVirtual)) && includedByFilter
    then do
      let amount = evaluatedPostingAmount evaluatedPosting
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
  -- | Daily price graphs (pre-computed from evaluated ledger)
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  -- | Price declarations from the ledger
  Vector (GenLocated ann (Price ann)) ->
  -- | Flat register to convert
  FlatRegister ann ->
  Validation (RegisterError ann) (ConvertedFlatRegister ann)
convertFlatRegister currencyTo mEnd prices pricesVec FlatRegister {..} = do
  let lastPriceDay = case M.lookupMax prices of
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
      go _convertedRunning rawBalances mLastDay [] =
        case (mLastDay, effectiveLastPriceDay) of
          (Just lastEntryDay, Just lastPDay)
            | lastEntryDay < lastPDay ->
                map ConvertedFlatEntryRevaluation
                  <$> computeRevaluations lastEntryDay lastPDay prices pricesVec currencyTo rawBalances
          _ -> pure []
      go convertedRunning rawBalances mLastDay (entry : rest) = do
        let Located _ ts = flatEntryTimestamp entry
            day = Timestamp.toDay ts
            currentPrices = flatEntryPriceGraph entry

        revalEntries <- case mLastDay of
          Nothing -> pure []
          Just lastDay ->
            if lastDay < day
              then computeRevaluations lastDay day prices pricesVec currencyTo rawBalances
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
convertFlatEntry currencyTo priceGraph _convertedRunning rawBalances FlatEntry {..} = do
  let goPostings ::
        Money.MultiAccount (Currency ann) ->
        [FlatPosting ann] ->
        Validation
          (RegisterError ann)
          ([ConvertedFlatPosting ann], Money.MultiAccount (Currency ann))
      goPostings rawBal [] = pure ([], rawBal)
      goPostings rawBal (fp : fps) = do
        (converted, newRawBal) <- convertFlatPosting currencyTo priceGraph rawBal fp
        (rest, finalRawBal) <- goPostings newRawBal fps
        pure (converted : rest, finalRawBal)

  (postings, newRawBalances) <-
    goPostings rawBalances (V.toList flatEntryPostings)

  -- Get the final converted running total from the last posting
  let newConvertedRunning = case postings of
        [] -> Account.zero
        _ -> convertedFlatPostingRunningTotal (last postings)

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
  Money.MultiAccount (Currency ann) ->
  FlatPosting ann ->
  Validation
    (RegisterError ann)
    (ConvertedFlatPosting ann, Money.MultiAccount (Currency ann))
convertFlatPosting currencyTo priceGraph rawBalances FlatPosting {..} = do
  let Located pl Posting {..} = flatPosting
      Located al _ = postingAccount

  -- Convert the amount
  converted <-
    mapValidationFailure RegisterErrorConvertError $
      convertMultiAccountToAccount (Just al) priceGraph currencyTo flatPostingAmount

  -- Update raw balances
  newRawBalances <- case MultiAccount.add rawBalances flatPostingAmount of
    Nothing -> validationFailure RegisterErrorAddError
    Just r -> pure r

  -- Convert the entire raw balance to get the running total.
  -- This avoids accumulating rounding errors from individual conversions.
  newConvertedRunning <-
    mapValidationFailure RegisterErrorConvertError $
      convertMultiAccountToAccount Nothing priceGraph currencyTo newRawBalances

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
  Validation (RegisterError ann) [ConvertedFlatRevaluation ann]
computeRevaluations lastDay currentDay priceGraphs allPrices currencyTo rawBalances = do
  let relevantPrices =
        V.toList $
          V.filter
            ( \(Located _ Price {..}) ->
                let Located _ ts = priceTimestamp
                    day = Timestamp.toDay ts
                 in day > lastDay && day <= currentDay
            )
            allPrices

  -- Compute initial converted total using the graph from before this period
  let initialPG = case M.lookupLE lastDay priceGraphs of
        Nothing -> MemoisedPriceGraph.empty
        Just (_, pg) -> pg
  initialConvertedTotal <-
    mapValidationFailure RegisterErrorConvertError $
      convertMultiAccountToAccount Nothing initialPG currencyTo rawBalances

  goRevals initialConvertedTotal relevantPrices
  where
    goRevals ::
      Money.Account ->
      [GenLocated ann (Price ann)] ->
      Validation (RegisterError ann) [ConvertedFlatRevaluation ann]
    goRevals _ [] = pure []
    goRevals prevConvertedTotal (Located _ firstPrice : rest) = do
      let Located _ ts = priceTimestamp firstPrice
          priceDay = Timestamp.toDay ts
          newPG = case M.lookupLE priceDay priceGraphs of
            Nothing -> MemoisedPriceGraph.empty
            Just (_, pg) -> pg

      -- Re-convert all balances with the new price graph and compare to previous total.
      -- This correctly handles multi-hop conversions (e.g., SWDA -> GBP -> CHF).
      newConvertedTotal <-
        mapValidationFailure RegisterErrorConvertError $
          convertMultiAccountToAccount Nothing newPG currencyTo rawBalances

      case Account.subtract newConvertedTotal prevConvertedTotal of
        Nothing -> validationFailure RegisterErrorAddError
        Just totalRevalAmount -> do
          -- The revaluation amount shown is the difference between converted totals.
          -- However, for the running total we use newConvertedTotal directly, not
          -- running + totalRevalAmount, to avoid accumulating rounding errors from
          -- individual posting conversions.
          let newRunning = newConvertedTotal
          -- Collect any following same-day prices that have zero difference
          -- (they share the same price graph, so their totals are identical)
          let (zeroCurrencies, restPrices) = collectZeroSameDayPrices priceDay newConvertedTotal rest
              allCurrencies = priceCurrency firstPrice :| zeroCurrencies
          let reval =
                ConvertedFlatRevaluation
                  { convertedFlatRevaluationTimestamp = priceTimestamp firstPrice,
                    convertedFlatRevaluationCurrencies = allCurrencies,
                    convertedFlatRevaluationAmount = totalRevalAmount,
                    convertedFlatRevaluationRunningTotal = newRunning
                  }
          restRevals <- goRevals newConvertedTotal restPrices
          pure (reval : restRevals)

    -- Collect same-day prices that would have zero revaluation amount
    collectZeroSameDayPrices ::
      Day ->
      Money.Account ->
      [GenLocated ann (Price ann)] ->
      ([GenLocated ann (Currency ann)], [GenLocated ann (Price ann)])
    collectZeroSameDayPrices _ _ [] = ([], [])
    collectZeroSameDayPrices day currentTotal prices@(Located _ nextPrice : rest)
      | Timestamp.toDay (locatedValue (priceTimestamp nextPrice)) /= day = ([], prices)
      | otherwise =
          -- Same day - check if the price graph gives the same total
          let nextPG = case M.lookupLE day priceGraphs of
                Nothing -> MemoisedPriceGraph.empty
                Just (_, pg) -> pg
           in case convertMultiAccountToAccount Nothing nextPG currencyTo rawBalances of
                Failure _ -> ([], prices) -- On error, don't group
                Success nextTotal
                  | nextTotal == currentTotal ->
                      -- Zero difference, include this currency and continue
                      let (moreCurrencies, remaining) = collectZeroSameDayPrices day currentTotal rest
                       in (priceCurrency nextPrice : moreCurrencies, remaining)
                  | otherwise -> ([], prices) -- Non-zero difference, don't group

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
          let (postings, newBlockRunning) = convertPostings blockRunning (V.toList (convertedFlatTransactionPostings t))
              regTx =
                RegisterTransaction
                  { registerTransactionTimestamp = convertedFlatTransactionTimestamp t,
                    registerTransactionDescription = convertedFlatTransactionDescription t,
                    registerTransactionPostings = V.fromList postings
                  }
           in (RegisterEntryTransaction regTx, newBlockRunning)
        ConvertedFlatEntryRevaluation r ->
          let revalAmount = convertedFlatRevaluationAmount r
              newBlockRunning = fromMaybe blockRunning (Account.add blockRunning revalAmount)
           in (RegisterEntryRevaluation (convertReval newBlockRunning r), newBlockRunning)

      convertReval ::
        Money.Account ->
        ConvertedFlatRevaluation ann ->
        RegisterRevaluation ann
      convertReval blockRunning r =
        let globalRunningTotal = convertedFlatRevaluationRunningTotal r
            -- For BlockSizeIndividual, use global running total (no separate "Total" column)
            displayedBlockRunning = case blockSize of
              BlockSizeIndividual -> globalRunningTotal
              _ -> blockRunning
         in RegisterRevaluation
              { registerRevaluationTimestamp = convertedFlatRevaluationTimestamp r,
                registerRevaluationCurrencies = convertedFlatRevaluationCurrencies r,
                registerRevaluationAmount = convertedFlatRevaluationAmount r,
                registerRevaluationRunningTotal = globalRunningTotal,
                registerRevaluationBlockRunningTotal = displayedBlockRunning
              }

      convertPostings ::
        Money.Account ->
        [ConvertedFlatPosting ann] ->
        ([RegisterPosting 'SingleCurrency ann], Money.Account)
      convertPostings blockRunning [] = ([], blockRunning)
      convertPostings blockRunning (cp : cps) =
        let -- Within-block running total: sum of individual amounts (for display within block)
            newBlockRunning = fromMaybe blockRunning (Account.add blockRunning (convertedFlatPostingAmount cp))
            -- Global running total: bulk-converted value (for final totals matching balance)
            globalRunningTotal = convertedFlatPostingRunningTotal cp
            -- For BlockSizeIndividual, use global running total (no separate "Total" column)
            -- For other block sizes, use within-block running total (separate "Total" column shows global)
            displayedBlockRunning = case blockSize of
              BlockSizeIndividual -> globalRunningTotal
              _ -> newBlockRunning
            regPosting =
              RegisterPosting
                { registerPosting = convertedFlatPosting cp,
                  registerPostingAmount = convertedFlatPostingAmount cp,
                  registerPostingRunningTotal = globalRunningTotal,
                  registerPostingBlockRunningTotal = displayedBlockRunning
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
                convertedEntries = convertEntriesForBlock blockEntries
                newRunningTotal = case blockEntries of
                  [] -> runningTotal
                  _ -> convertedFlatEntryRunningTotal (last blockEntries)
                -- Block total is the difference in running totals, not the sum of individual amounts.
                -- This avoids accumulating rounding errors from individual conversions.
                blockTotal = fromMaybe Account.zero (Account.subtract newRunningTotal runningTotal)
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
        [RegisterEntry 'SingleCurrency ann]
      convertEntriesForBlock entries =
        let go _ [] = []
            go blockRunning (e : es) =
              let (converted, newBlockRunning) = convertEntry blockRunning e
               in converted : go newBlockRunning es
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

-- | Build a map from Day to the cumulative price graph at that day,
-- extracted from the evaluated ledger's price entries.
buildDailyPriceGraphsFromEntries ::
  Vector (EvaluatedEntry ann) ->
  Map Day (MemoisedPriceGraph (Currency ann))
buildDailyPriceGraphsFromEntries = V.foldl' go M.empty
  where
    go m = \case
      EvaluatedEntryPrice evaluatedPrice ->
        let Located _ p = evaluatedPriceLocated evaluatedPrice
            Located _ ts = priceTimestamp p
            day = Timestamp.toDay ts
         in M.insert day (evaluatedPricePriceGraph evaluatedPrice) m
      EvaluatedEntryTransaction _ -> m
