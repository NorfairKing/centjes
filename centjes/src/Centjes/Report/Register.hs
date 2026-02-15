{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Register
  ( Register (..),
    RegisterBlock (..),
    RegisterEntry (..),
    RegisterTransaction (..),
    RegisterRevaluation (..),
    RegisterPosting (..),
    BlockSize (..),
    RegisterError (..),
    produceRegister,
  )
where

import Centjes.Block
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation (ToReport (..), Validation (..), mapValidationFailure, validationFailure)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Validity (Validity (..), declare, genericValidate)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Rounding (..))
import qualified Money.MultiAccount as Money (MultiAccount (..))
import qualified Money.MultiAccount as MultiAccount

data Register ann = Register
  { registerBlockSize :: !BlockSize,
    registerBlocks :: !(Vector (RegisterBlock ann)),
    registerTotal :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Register ann) where
  validate r@Register {..} =
    mconcat
      [ genericValidate r,
        declare "The total of the register matches the sum of the blocks" $
          MultiAccount.sum (fmap registerBlockTotal registerBlocks)
            == Just registerTotal
      ]

data RegisterBlock ann = RegisterBlock
  { registerBlockTitle :: !Block,
    registerBlockEntries :: !(Vector (RegisterEntry ann)),
    -- Total of this block
    registerBlockTotal :: !(Money.MultiAccount (Currency ann)),
    -- Running accross blocks
    registerBlockRunningTotal :: !(Money.MultiAccount (Currency ann)),
    -- Running average accross blocks
    registerBlockRunningAverage :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterBlock ann) where
  validate rb@RegisterBlock {..} =
    mconcat
      [ genericValidate rb,
        declare "The total of the block matches the sum of the entries" $
          (mapM registerEntryTotal registerBlockEntries >>= MultiAccount.sum)
            == Just registerBlockTotal
      ]

-- | A register entry is either a transaction or a revaluation
data RegisterEntry ann
  = RegisterEntryTransaction !(RegisterTransaction ann)
  | RegisterEntryRevaluation !(RegisterRevaluation ann)
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterEntry ann)

-- | Get the total amount for a register entry
registerEntryTotal :: (Ord ann) => RegisterEntry ann -> Maybe (Money.MultiAccount (Currency ann))
registerEntryTotal = \case
  RegisterEntryTransaction rt -> registerTransactionTotal rt
  RegisterEntryRevaluation rr -> Just (registerRevaluationAmount rr)

-- | A revaluation entry representing gain/loss due to price changes
data RegisterRevaluation ann = RegisterRevaluation
  { registerRevaluationTimestamp :: !(GenLocated ann Timestamp),
    -- | The currency whose price changed
    registerRevaluationCurrency :: !(GenLocated ann (Currency ann)),
    -- | The gain or loss amount
    registerRevaluationAmount :: !(Money.MultiAccount (Currency ann)),
    -- | Running total across blocks after this revaluation
    registerRevaluationRunningTotal :: !(Money.MultiAccount (Currency ann)),
    -- | Running total within the block after this revaluation
    registerRevaluationBlockRunningTotal :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterRevaluation ann)

data RegisterTransaction ann = RegisterTransaction
  { registerTransactionTimestamp :: !(GenLocated ann Timestamp),
    registerTransactionDescription :: !(Maybe (GenLocated ann Description)),
    registerTransactionPostings :: !(Vector (RegisterPosting ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterTransaction ann)

registerTransactionTotal :: (Ord ann) => RegisterTransaction ann -> Maybe (Money.MultiAccount (Currency ann))
registerTransactionTotal =
  MultiAccount.sum
    . fmap registerPostingMultiAccount
    . registerTransactionPostings

data RegisterPosting ann = RegisterPosting
  { registerPosting :: !(GenLocated ann (Posting ann)),
    -- Running total accross blocks
    registerPostingRunningTotal :: !(Money.MultiAccount (Currency ann)),
    -- Running total within the block
    registerPostingBlockRunningTotal :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterPosting ann)

registerPostingMultiAccount :: RegisterPosting ann -> Money.MultiAccount (Currency ann)
registerPostingMultiAccount RegisterPosting {..} = MultiAccount.fromAccount currency account
  where
    Located _ Posting {..} = registerPosting
    Located _ currency = postingCurrency
    Located _ account = postingAccount

data RegisterError ann
  = RegisterErrorAddError
  | RegisterErrorConvertError !(ConvertError ann)
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterErrorAddError -> undefined
    RegisterErrorConvertError ce -> toReport ce

-- | An event in the ledger, either a transaction or a price declaration
data LedgerEvent ann
  = EventTransaction !(GenLocated ann (Transaction ann))
  | EventPrice !(GenLocated ann (Price ann))

-- | Get the timestamp of a ledger event
eventTimestamp :: LedgerEvent ann -> GenLocated ann Timestamp
eventTimestamp = \case
  EventTransaction (Located _ t) -> transactionTimestamp t
  EventPrice (Located _ p) -> priceTimestamp p

-- | Get the day of a ledger event
eventDay :: LedgerEvent ann -> Day
eventDay = Timestamp.toDay . locatedValue . eventTimestamp

-- | Compare two events by timestamp, with a total ordering
-- When timestamps are equal or ambiguous (same day, no time info),
-- we treat prices as coming before transactions on the same day
-- (so revaluations are computed before new transactions)
compareEvents :: LedgerEvent ann -> LedgerEvent ann -> Ordering
compareEvents e1 e2 =
  let ts1 = locatedValue (eventTimestamp e1)
      ts2 = locatedValue (eventTimestamp e2)
   in case Timestamp.comparePartially ts1 ts2 of
        Just EQ -> eventTypeTiebreaker e1 e2
        Just ord -> ord
        Nothing ->
          -- Same day but ambiguous time - compare days, then event type
          case compare (eventDay e1) (eventDay e2) of
            EQ -> eventTypeTiebreaker e1 e2
            ord -> ord

-- | Tiebreaker for events on the same day: prices come before transactions
eventTypeTiebreaker :: LedgerEvent ann -> LedgerEvent ann -> Ordering
eventTypeTiebreaker e1 e2 = case (e1, e2) of
  (EventPrice _, EventTransaction _) -> LT
  (EventTransaction _, EventPrice _) -> GT
  _ -> EQ

-- | Merge transactions and prices into a chronologically sorted list of events
mergeEventsChronologically ::
  Vector (GenLocated ann (Transaction ann)) ->
  Vector (GenLocated ann (Price ann)) ->
  [LedgerEvent ann]
mergeEventsChronologically transactions prices =
  let txEvents = map EventTransaction (V.toList transactions)
      priceEvents = map EventPrice (V.toList prices)
   in sortBy compareEvents (txEvents ++ priceEvents)

-- | Raw entry before block grouping (no block-scoped totals)
data RawRegisteredEntry ann
  = RawRegisteredEntryTransaction !(RawRegisteredTransaction ann)
  | RawRegisteredEntryRevaluation !(RawRegisteredRevaluation ann)

data RawRegisteredTransaction ann = RawRegisteredTransaction
  { rawTransactionTimestamp :: !(GenLocated ann Timestamp),
    rawTransactionDescription :: !(Maybe (GenLocated ann Description)),
    rawTransactionPostings :: !(Vector (RawRegisteredPosting ann))
  }

data RawRegisteredPosting ann = RawRegisteredPosting
  { rawPosting :: !(GenLocated ann (Posting ann)),
    -- | Running total across blocks (not block-scoped)
    rawPostingRunningTotal :: !(Money.MultiAccount (Currency ann))
  }

data RawRegisteredRevaluation ann = RawRegisteredRevaluation
  { rawRevaluationTimestamp :: !(GenLocated ann Timestamp),
    rawRevaluationCurrency :: !(GenLocated ann (Currency ann)),
    rawRevaluationAmount :: !(Money.MultiAccount (Currency ann)),
    -- | Running total across blocks (not block-scoped)
    rawRevaluationRunningTotal :: !(Money.MultiAccount (Currency ann))
  }

-- | Simplified state for Phase 1 (no block running totals)
data RegisteringState ann = RegisteringState
  { registeringStateConvertedRunning :: !(Money.MultiAccount (Currency ann)),
    registeringStateRawBalances :: !(Money.MultiAccount (Currency ann))
  }

initialRegisteringState :: RegisteringState ann
initialRegisteringState =
  RegisteringState
    { registeringStateConvertedRunning = MultiAccount.zero,
      registeringStateRawBalances = MultiAccount.zero
    }

-- | Output of Phase 1
data RegisteredEntries ann = RegisteredEntries
  { registeredEntries :: ![RawRegisteredEntry ann],
    registeredTotal :: !(Money.MultiAccount (Currency ann))
  }

produceRegister ::
  forall ann.
  (Ord ann) =>
  Filter ->
  BlockSize ->
  Maybe CurrencySymbol ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Ledger ann ->
  Validation (RegisterError ann) (Register ann)
produceRegister f blockSize mCurrencySymbolTo showVirtual mBegin mEnd ledger = do
  -- Phase 1: Register entries chronologically
  entries <- registerEntries f mCurrencySymbolTo showVirtual mBegin mEnd ledger
  -- Phase 2: Group into blocks
  groupIntoBlocks blockSize mBegin mEnd entries

-- | Phase 1: Process transactions and prices chronologically, tracking running balances
registerEntries ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Maybe CurrencySymbol ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Ledger ann ->
  Validation (RegisterError ann) (RegisteredEntries ann)
registerEntries f mCurrencySymbolTo showVirtual mBegin mEnd ledger = do
  mCurrencyTo <-
    mapValidationFailure RegisterErrorConvertError $
      traverse (lookupConversionCurrency (ledgerCurrencies ledger)) mCurrencySymbolTo
  let prices = pricesToDailyPriceGraphs (ledgerPrices ledger)
  -- Merge transactions and prices into chronologically sorted events
  let events = mergeEventsChronologically (ledgerTransactions ledger) (ledgerPrices ledger)

  let go ::
        RegisteringState ann ->
        [LedgerEvent ann] ->
        Validation (RegisterError ann) [RawRegisteredEntry ann]
      go _ [] = pure []
      go state (evt : evts) = do
        mEntry <-
          registerEvent
            f
            showVirtual
            mBegin
            mEnd
            mCurrencyTo
            prices
            state
            evt
        case mEntry of
          Nothing -> go state evts
          Just (entry, newState) -> do
            rest <- go newState evts
            pure (entry : rest)

  entries <- go initialRegisteringState events
  let total = case entries of
        [] -> MultiAccount.zero
        _ -> rawEntryRunningTotal (last entries)
  pure
    RegisteredEntries
      { registeredEntries = entries,
        registeredTotal = total
      }

-- | Get the running total from a raw entry
rawEntryRunningTotal :: RawRegisteredEntry ann -> Money.MultiAccount (Currency ann)
rawEntryRunningTotal = \case
  RawRegisteredEntryTransaction rt ->
    case V.toList (rawTransactionPostings rt) of
      [] -> MultiAccount.zero
      ps -> rawPostingRunningTotal (last ps)
  RawRegisteredEntryRevaluation rr -> rawRevaluationRunningTotal rr

-- | Process a single ledger event and produce a raw entry if applicable
registerEvent ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  RegisteringState ann ->
  LedgerEvent ann ->
  Validation
    (RegisterError ann)
    (Maybe (RawRegisteredEntry ann, RegisteringState ann))
registerEvent f showVirtual mBegin mEnd mCurrencyTo prices state = \case
  EventTransaction tx -> do
    mResult <- registerTransaction f showVirtual mBegin mEnd mCurrencyTo prices state tx
    pure $ case mResult of
      Nothing -> Nothing
      Just (rt, newState) -> Just (RawRegisteredEntryTransaction rt, newState)
  EventPrice lp -> do
    -- Only process price events when we have a conversion currency
    case mCurrencyTo of
      Nothing -> pure Nothing
      Just currencyTo ->
        registerPriceEvent mBegin mEnd prices currencyTo state lp

-- | Process a transaction into a raw registered transaction
registerTransaction ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  RegisteringState ann ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RawRegisteredTransaction ann, RegisteringState ann))
registerTransaction
  f
  showVirtual
  mBegin
  mEnd
  mCurrencyTo
  prices
  state
  (Located _ Transaction {..}) =
    if timestampPassesDayFilter mBegin mEnd transactionTimestamp
      then do
        let runningTotal = registeringStateConvertedRunning state
            rawBalances = registeringStateRawBalances state
        let goPostings running rawBal = \case
              [] -> pure ([], running, rawBal)
              posting : rest -> do
                let Located _ ts = transactionTimestamp
                    day = Timestamp.toDay ts
                    dayPrices = case M.lookupLE day prices of
                      Nothing -> MemoisedPriceGraph.empty
                      Just (_, pg) -> pg
                mPosting <-
                  processPosting
                    f
                    showVirtual
                    mCurrencyTo
                    dayPrices
                    running
                    posting
                case mPosting of
                  Nothing -> goPostings running rawBal rest
                  Just (rawPosting, rawDelta) -> do
                    -- Update raw balances with the unconverted amount
                    newRawBal <- case MultiAccount.add rawBal rawDelta of
                      Nothing -> validationFailure RegisterErrorAddError
                      Just r -> pure r
                    (ps, finalRunning, finalRawBal) <-
                      goPostings
                        (rawPostingRunningTotal rawPosting)
                        newRawBal
                        rest
                    pure (rawPosting : ps, finalRunning, finalRawBal)
        (registeredPostings, newRunningTotal, newRawBalances) <-
          goPostings runningTotal rawBalances (V.toList transactionPostings)
        let rawTx =
              RawRegisteredTransaction
                { rawTransactionTimestamp = transactionTimestamp,
                  rawTransactionDescription = transactionDescription,
                  rawTransactionPostings = V.fromList registeredPostings
                }
        pure $
          if null registeredPostings
            then Nothing
            else
              Just
                ( rawTx,
                  RegisteringState
                    { registeringStateConvertedRunning = newRunningTotal,
                      registeringStateRawBalances = newRawBalances
                    }
                )
      else pure Nothing

-- | Process a posting into a raw registered posting
processPosting ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe (Currency ann) ->
  MemoisedPriceGraph (Currency ann) ->
  Money.MultiAccount (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RawRegisteredPosting ann, Money.MultiAccount (Currency ann)))
processPosting
  f
  showVirtual
  mCurrencyTo
  prices
  runningTotal
  originalPosting@(Located _ origP) = do
    -- Get the raw (unconverted) amount from the original posting
    let Located _ origCurrency = postingCurrency origP
        Located _ origAccount = postingAccount origP
        rawAmount = MultiAccount.fromAccount origCurrency origAccount
    mPosting <- convertPosting f showVirtual mCurrencyTo prices originalPosting
    case mPosting of
      Nothing -> pure Nothing
      Just lposting@(Located _ Posting {..}) -> do
        let Located _ currency = postingCurrency
        let Located _ account = postingAccount
        let ma = MultiAccount.fromAccount currency account

        case MultiAccount.add runningTotal ma of
          Nothing -> validationFailure RegisterErrorAddError
          Just rawPostingRunningTotal ->
            pure $
              Just
                ( RawRegisteredPosting
                    { rawPosting = lposting,
                      rawPostingRunningTotal = rawPostingRunningTotal
                    },
                  rawAmount
                )

-- | Process a price event to potentially create a raw revaluation entry
registerPriceEvent ::
  forall ann.
  (Ord ann) =>
  Maybe Day ->
  Maybe Day ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Currency ann ->
  RegisteringState ann ->
  GenLocated ann (Price ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RawRegisteredEntry ann, RegisteringState ann))
registerPriceEvent mBegin mEnd prices currencyTo state (Located _ Price {..}) = do
  let Located _ ts = priceTimestamp
      day = Timestamp.toDay ts
      -- A price declaration affects both currencies involved
      Located _ pricedCurrency = priceCurrency
      Located _ Cost {..} = priceCost
      Located _ costCur = costCurrency
  -- Check if this price is within the date filter
  if not (dayPassesDayFilter mBegin mEnd day)
    then pure Nothing
    else do
      let rawBalances = registeringStateRawBalances state
      -- Look at balances in both currencies this price affects
      let pricedBalance = MultiAccount.lookupAccount pricedCurrency rawBalances
          costBalance = MultiAccount.lookupAccount costCur rawBalances
      -- If we have no balance in either affected currency, no revaluation needed
      if pricedBalance == Account.zero && costBalance == Account.zero
        then pure Nothing
        else do
          -- Create a MultiAccount for just the affected currencies
          let affectedRawBalance =
                case MultiAccount.addAccount (MultiAccount.fromAccount pricedCurrency pricedBalance) costCur costBalance of
                  Nothing -> MultiAccount.fromAccount pricedCurrency pricedBalance
                  Just ma -> ma
          -- Get the price graph BEFORE this price was added
          let oldPrices = case M.lookupLT day prices of
                Nothing -> MemoisedPriceGraph.empty
                Just (_, pg) -> pg
          -- Get the price graph AFTER this price (which includes it)
          let newPrices = case M.lookupLE day prices of
                Nothing -> MemoisedPriceGraph.empty
                Just (_, pg) -> pg
          -- Try to convert the affected balances with old prices
          let oldConvertedResult =
                mapValidationFailure RegisterErrorConvertError $
                  convertMultiAccount Nothing oldPrices currencyTo affectedRawBalance
              oldConverted = case oldConvertedResult of
                Failure _ -> MultiAccount.zero
                Success v -> v
          -- Convert the affected balances with new prices
          newConverted <-
            mapValidationFailure RegisterErrorConvertError $
              convertMultiAccount Nothing newPrices currencyTo affectedRawBalance
          -- Calculate the difference (revaluation amount)
          case MultiAccount.subtract newConverted oldConverted of
            Nothing -> validationFailure RegisterErrorAddError
            Just revaluationAmount ->
              -- Only create a revaluation entry if there's a non-zero change
              if revaluationAmount == MultiAccount.zero
                then pure Nothing
                else do
                  -- Update the running total
                  case MultiAccount.add (registeringStateConvertedRunning state) revaluationAmount of
                    Nothing -> validationFailure RegisterErrorAddError
                    Just newRunning -> do
                      let reval =
                            RawRegisteredRevaluation
                              { rawRevaluationTimestamp = priceTimestamp,
                                rawRevaluationCurrency = priceCurrency,
                                rawRevaluationAmount = revaluationAmount,
                                rawRevaluationRunningTotal = newRunning
                              }
                      let newState =
                            state
                              { registeringStateConvertedRunning = newRunning
                              }
                      pure $ Just (RawRegisteredEntryRevaluation reval, newState)

-- | Phase 2: Group entries into blocks, compute block totals and running averages
groupIntoBlocks ::
  forall ann.
  (Ord ann) =>
  BlockSize ->
  Maybe Day ->
  Maybe Day ->
  RegisteredEntries ann ->
  Validation (RegisterError ann) (Register ann)
groupIntoBlocks blockSize mBegin mEnd RegisteredEntries {..} = do
  let mBeginBlock = (\begin -> dayBlockTitle begin blockSize) <$> mBegin
      mEndBlock = (\end -> dayBlockTitle end blockSize) <$> mEnd

  let -- Convert a raw entry to a register entry with block running totals
      convertEntry ::
        Money.MultiAccount (Currency ann) ->
        RawRegisteredEntry ann ->
        (RegisterEntry ann, Money.MultiAccount (Currency ann))
      convertEntry blockRunning = \case
        RawRegisteredEntryTransaction rt ->
          let (postings, finalBlockRunning) = convertPostings blockRunning (V.toList (rawTransactionPostings rt))
              regTx =
                RegisterTransaction
                  { registerTransactionTimestamp = rawTransactionTimestamp rt,
                    registerTransactionDescription = rawTransactionDescription rt,
                    registerTransactionPostings = V.fromList postings
                  }
           in (RegisterEntryTransaction regTx, finalBlockRunning)
        RawRegisteredEntryRevaluation rr ->
          case MultiAccount.add blockRunning (rawRevaluationAmount rr) of
            Nothing -> (RegisterEntryRevaluation (convertReval blockRunning rr), blockRunning)
            Just newBlockRunning ->
              (RegisterEntryRevaluation (convertReval newBlockRunning rr), newBlockRunning)

      convertReval ::
        Money.MultiAccount (Currency ann) ->
        RawRegisteredRevaluation ann ->
        RegisterRevaluation ann
      convertReval blockRunning rr =
        RegisterRevaluation
          { registerRevaluationTimestamp = rawRevaluationTimestamp rr,
            registerRevaluationCurrency = rawRevaluationCurrency rr,
            registerRevaluationAmount = rawRevaluationAmount rr,
            registerRevaluationRunningTotal = rawRevaluationRunningTotal rr,
            registerRevaluationBlockRunningTotal = blockRunning
          }

      convertPostings ::
        Money.MultiAccount (Currency ann) ->
        [RawRegisteredPosting ann] ->
        ([RegisterPosting ann], Money.MultiAccount (Currency ann))
      convertPostings blockRunning [] = ([], blockRunning)
      convertPostings blockRunning (rp : rps) =
        let Located _ Posting {..} = rawPosting rp
            Located _ currency = postingCurrency
            Located _ account = postingAccount
            ma = MultiAccount.fromAccount currency account
            newBlockRunning = fromMaybe blockRunning (MultiAccount.add blockRunning ma)
            regPosting =
              RegisterPosting
                { registerPosting = rawPosting rp,
                  registerPostingRunningTotal = rawPostingRunningTotal rp,
                  registerPostingBlockRunningTotal = newBlockRunning
                }
            (rest, finalBlockRunning) = convertPostings newBlockRunning rps
         in (regPosting : rest, finalBlockRunning)

  let -- Get timestamp of a raw entry
      rawEntryTimestamp :: RawRegisteredEntry ann -> GenLocated ann Timestamp
      rawEntryTimestamp = \case
        RawRegisteredEntryTransaction rt -> rawTransactionTimestamp rt
        RawRegisteredEntryRevaluation rr -> rawRevaluationTimestamp rr

      -- Get block title for a raw entry
      rawEntryBlockTitle :: RawRegisteredEntry ann -> Block
      rawEntryBlockTitle entry =
        let Located _ ts = rawEntryTimestamp entry
         in timestampBlockTitle ts blockSize

  let -- Build blocks from entries
      goBlocks ::
        Maybe Block ->
        Integer ->
        Money.MultiAccount (Currency ann) ->
        [RawRegisteredEntry ann] ->
        [RegisterBlock ann]
      goBlocks mCurrentBlock blockNum runningTotal entries =
        case findNextBlock mCurrentBlock entries of
          Nothing ->
            -- No more entries - generate empty blocks if we have an end block
            case (mCurrentBlock, mEndBlock) of
              (Just current, Just endBlock)
                | current <= endBlock ->
                    let emptyBlock = makeEmptyBlock current runningTotal blockNum
                     in emptyBlock : goBlocks (Just (nextBlock current)) blockNum runningTotal []
              _ -> []
          Just (blockTitle, blockEntries, restEntries) ->
            -- Only generate empty blocks when we have an end block
            let emptyBlocks = case (mCurrentBlock, mEndBlock) of
                  (Just current, Just _)
                    | current < blockTitle ->
                        generateEmptyBlocks current blockTitle runningTotal blockNum
                  _ -> []
                numEmpty = toInteger (length emptyBlocks)
                -- Convert entries with block running totals
                (convertedEntries, blockTotal) =
                  convertEntriesForBlock blockEntries
                newRunningTotal = case entries of
                  [] -> runningTotal
                  _ -> rawEntryRunningTotal (last blockEntries)
                hasTransactions = any isRawTransactionEntry blockEntries
                effectiveBlockNum = blockNum + numEmpty
                nextBlockNum = if hasTransactions then effectiveBlockNum + 1 else effectiveBlockNum
                block =
                  RegisterBlock
                    { registerBlockTitle = blockTitle,
                      registerBlockEntries = V.fromList convertedEntries,
                      registerBlockTotal = blockTotal,
                      registerBlockRunningTotal = newRunningTotal,
                      registerBlockRunningAverage =
                        computeAverage newRunningTotal (if hasTransactions then effectiveBlockNum else max 1 (effectiveBlockNum - 1))
                    }
                -- Check if we should continue
                shouldContinue = case mEndBlock of
                  Nothing -> not (null restEntries)
                  Just endBlock -> blockTitle < endBlock
             in if shouldContinue
                  then emptyBlocks ++ [block] ++ goBlocks (Just (nextBlock blockTitle)) nextBlockNum newRunningTotal restEntries
                  else emptyBlocks ++ [block]

      -- Find the next block from entries
      findNextBlock ::
        Maybe Block ->
        [RawRegisteredEntry ann] ->
        Maybe (Block, [RawRegisteredEntry ann], [RawRegisteredEntry ann])
      findNextBlock mCurrentBlock entries = case entries of
        [] -> Nothing
        (e : _) ->
          let blockTitle = rawEntryBlockTitle e
              -- Check if we need to skip to this block or generate empty blocks
              shouldProcess = case mCurrentBlock of
                Nothing -> True
                Just current -> blockTitle >= current
           in if shouldProcess
                then
                  let (blockEntries, rest) = span (\entry -> rawEntryBlockTitle entry == blockTitle) entries
                   in Just (blockTitle, blockEntries, rest)
                else -- Skip entries before the current block
                  findNextBlock mCurrentBlock (dropWhile (\entry -> rawEntryBlockTitle entry < fromMaybe blockTitle mCurrentBlock) entries)

      -- Generate empty blocks between two blocks
      generateEmptyBlocks ::
        Block ->
        Block ->
        Money.MultiAccount (Currency ann) ->
        Integer ->
        [RegisterBlock ann]
      generateEmptyBlocks current target runningTotal blockNum
        | current >= target = []
        | otherwise =
            let emptyBlock = makeEmptyBlock current runningTotal blockNum
             in emptyBlock : generateEmptyBlocks (nextBlock current) target runningTotal blockNum

      -- Make an empty block
      makeEmptyBlock ::
        Block ->
        Money.MultiAccount (Currency ann) ->
        Integer ->
        RegisterBlock ann
      makeEmptyBlock title runningTotal blockNum =
        RegisterBlock
          { registerBlockTitle = title,
            registerBlockEntries = V.empty,
            registerBlockTotal = MultiAccount.zero,
            registerBlockRunningTotal = runningTotal,
            registerBlockRunningAverage = computeAverage runningTotal blockNum
          }

      -- Convert entries for a block, computing block running totals
      convertEntriesForBlock ::
        [RawRegisteredEntry ann] ->
        ([RegisterEntry ann], Money.MultiAccount (Currency ann))
      convertEntriesForBlock entries =
        let go _ [] = ([], MultiAccount.zero)
            go blockRunning (e : es) =
              let (converted, newBlockRunning) = convertEntry blockRunning e
                  (rest, restTotal) = go newBlockRunning es
                  entryTotal = fromMaybe MultiAccount.zero (registerEntryTotal converted)
                  combinedTotal = fromMaybe entryTotal (MultiAccount.add entryTotal restTotal)
               in (converted : rest, combinedTotal)
         in go MultiAccount.zero entries

      -- Check if entry is a transaction (for averaging)
      isRawTransactionEntry :: RawRegisteredEntry ann -> Bool
      isRawTransactionEntry = \case
        RawRegisteredEntryTransaction _ -> True
        RawRegisteredEntryRevaluation _ -> False

  let blocks = goBlocks mBeginBlock 1 MultiAccount.zero registeredEntries
  pure
    Register
      { registerBlockSize = blockSize,
        registerBlocks = V.fromList blocks,
        registerTotal = registeredTotal
      }

computeAverage :: Money.MultiAccount (Currency ann) -> Integer -> Money.MultiAccount (Currency ann)
computeAverage (Money.MultiAccount m) blockNum =
  Money.MultiAccount $ M.map go m
  where
    go a =
      let (ma, _) = Account.fraction Money.RoundDown a (1 % blockNum)
       in -- TODO technically this should fail instead of being zero, I think?
          fromMaybe Account.zero ma

dayPassesDayFilter :: Maybe Day -> Maybe Day -> Day -> Bool
dayPassesDayFilter mBegin mEnd day =
  and
    [ case mBegin of
        Nothing -> True
        Just begin -> day >= begin,
      case mEnd of
        Nothing -> True
        Just end -> day <= end
    ]

timestampPassesDayFilter :: Maybe Day -> Maybe Day -> GenLocated ann Timestamp -> Bool
timestampPassesDayFilter mBegin mEnd (Located _ ts) =
  and
    [ case mBegin of
        Nothing -> True
        Just begin -> Timestamp.toDay ts >= begin,
      case mEnd of
        Nothing -> True
        Just end -> Timestamp.toDay ts <= end
    ]

convertPosting ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe (Currency ann) ->
  MemoisedPriceGraph (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation (RegisterError ann) (Maybe (GenLocated ann (Posting ann)))
convertPosting f showVirtual mCurrencyTo prices lp@(Located pl p@Posting {..}) = do
  let Located _ an = postingAccountName
  let includedByFilter = Filter.predicate f an
  -- Don't show virtual postings by default
  if (postingReal || (not postingReal && showVirtual)) && includedByFilter
    then
      Just <$> do
        let Located cl currency = postingCurrency
        let Located al account = postingAccount
        case mCurrencyTo of
          Nothing -> pure lp
          Just currencyTo -> do
            converted <-
              mapValidationFailure
                RegisterErrorConvertError
                ( convertMultiAccountToAccount
                    (Just al)
                    prices
                    currencyTo
                    (MultiAccount.fromAccount currency account)
                )
            pure $
              Located
                pl
                p
                  { postingCurrency = Located cl currencyTo,
                    postingAccount = Located al converted
                  }
    else pure Nothing
