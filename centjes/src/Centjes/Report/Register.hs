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
import Control.Monad
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

-- | Check if an entry is a transaction (vs a revaluation)
isTransactionEntry :: RegisterEntry ann -> Bool
isTransactionEntry = \case
  RegisterEntryTransaction _ -> True
  RegisterEntryRevaluation _ -> False

-- | A revaluation entry representing gain/loss due to price changes
data RegisterRevaluation ann = RegisterRevaluation
  { registerRevaluationTimestamp :: !(GenLocated ann Timestamp),
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
  deriving (Show, Generic)

-- | Get the timestamp of a ledger event
eventTimestamp :: LedgerEvent ann -> GenLocated ann Timestamp
eventTimestamp = \case
  EventTransaction (Located _ t) -> transactionTimestamp t
  EventPrice (Located _ p) -> priceTimestamp p

-- | Get the day of a ledger event
eventDay :: LedgerEvent ann -> Day
eventDay = Timestamp.toDay . locatedValue . eventTimestamp

-- | Compare two events by timestamp, with a total ordering
-- When timestamps are ambiguous (partial ordering returns Nothing),
-- we compare by day first, then treat prices as coming before transactions
-- on the same day (so revaluations are computed before new transactions)
compareEvents :: LedgerEvent ann -> LedgerEvent ann -> Ordering
compareEvents e1 e2 =
  let ts1 = locatedValue (eventTimestamp e1)
      ts2 = locatedValue (eventTimestamp e2)
   in case Timestamp.comparePartially ts1 ts2 of
        Just ord -> ord
        Nothing ->
          -- Same day but ambiguous time - compare days, then event type
          -- Prices come before transactions on the same day
          case compare (eventDay e1) (eventDay e2) of
            EQ -> case (e1, e2) of
              (EventPrice _, EventTransaction _) -> LT
              (EventTransaction _, EventPrice _) -> GT
              _ -> EQ
            ord -> ord

-- | Merge transactions and prices into a chronologically sorted list of events
mergeEventsChronologically ::
  Vector (GenLocated ann (Transaction ann)) ->
  Vector (GenLocated ann (Price ann)) ->
  [LedgerEvent ann]
mergeEventsChronologically transactions prices =
  let txEvents = map EventTransaction (V.toList transactions)
      priceEvents = map EventPrice (V.toList prices)
   in sortBy compareEvents (txEvents ++ priceEvents)

-- | Running state for register computation
-- Tracks both converted running totals and raw (unconverted) balances
data RegisterState ann = RegisterState
  { rsConvertedRunning :: !(Money.MultiAccount (Currency ann)),
    rsConvertedBlockRunning :: !(Money.MultiAccount (Currency ann)),
    rsRawBalances :: !(Money.MultiAccount (Currency ann))
  }

initialRegisterState :: RegisterState ann
initialRegisterState =
  RegisterState
    { rsConvertedRunning = MultiAccount.zero,
      rsConvertedBlockRunning = MultiAccount.zero,
      rsRawBalances = MultiAccount.zero
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
  let registerBlockSize = blockSize
  mCurrencyTo <-
    mapValidationFailure RegisterErrorConvertError $
      traverse (lookupConversionCurrency (ledgerCurrencies ledger)) mCurrencySymbolTo
  let prices = pricesToDailyPriceGraphs (ledgerPrices ledger)
  let mBeginBlock = (\begin -> dayBlockTitle begin blockSize) <$> mBegin
  let mEndBlock = (\end -> dayBlockTitle end blockSize) <$> mEnd
  -- Merge transactions and prices into chronologically sorted events
  let events = mergeEventsChronologically (ledgerTransactions ledger) (ledgerPrices ledger)
  let goBlocks ::
        Maybe Block ->
        Integer ->
        RegisterState ann ->
        [LedgerEvent ann] ->
        Validation
          (RegisterError ann)
          ( [RegisterBlock ann],
            Money.MultiAccount (Currency ann)
          )
      goBlocks mCurrentBlock blockNum state evts = do
        (mBlock, restEvents, newState) <-
          tallyBlock
            f
            showVirtual
            mBegin
            mEnd
            blockSize
            mCurrentBlock
            mCurrencyTo
            prices
            blockNum
            state
            evts
        case mBlock of
          Nothing -> do
            let done = pure ([], rsConvertedRunning state)
            case mEndBlock of
              Nothing -> done
              Just endBlock -> case mCurrentBlock of
                Nothing -> done
                Just currentBlock ->
                  if currentBlock >= endBlock
                    then done
                    else goBlocks (Just (nextBlock currentBlock)) blockNum state evts
          Just block -> do
            let currentBlock = registerBlockTitle block
            let blockHasEntries = not (V.null (registerBlockEntries block))
            -- Only count blocks with transactions for averaging purposes
            let blockHasTransactions = V.any isTransactionEntry (registerBlockEntries block)
            let done = pure ([block], rsConvertedRunning newState)
                -- When continuing, only increment blockNum if the block has transactions
                continue keepEmptyBlocks = do
                  let nextBlockNum = if blockHasTransactions then succ blockNum else blockNum
                  (restBlocks, total) <-
                    goBlocks
                      (Just (nextBlock currentBlock))
                      nextBlockNum
                      newState
                      restEvents
                  -- Keep empty blocks if we have an end block (date range specified)
                  let includeBlock = blockHasEntries || keepEmptyBlocks
                  pure (if includeBlock then block : restBlocks else restBlocks, total)
             in case mEndBlock of
                  Nothing ->
                    -- Continue if the block has entries, or if there are remaining events to process
                    -- Don't keep empty blocks when there's no end block
                    if not blockHasEntries && null restEvents
                      then pure ([], rsConvertedRunning state)
                      else continue False
                  Just endBlock ->
                    if currentBlock >= endBlock
                      then done
                      -- Keep empty blocks when we have an end block (date range)
                      else continue True
  (blocks, registerTotal) <-
    goBlocks
      mBeginBlock
      1
      initialRegisterState
      events
  let registerBlocks = V.fromList blocks
  pure Register {..}

computeAverage :: Money.MultiAccount (Currency ann) -> Integer -> Money.MultiAccount (Currency ann)
computeAverage (Money.MultiAccount m) blockNum =
  Money.MultiAccount $ M.map go m
  where
    go a =
      let (ma, _) = Account.fraction Money.RoundDown a (1 % blockNum)
       in -- TODO technically this should fail instead of being zero, I think?
          fromMaybe Account.zero ma

tallyBlock ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  BlockSize ->
  Maybe Block ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Integer ->
  RegisterState ann ->
  [LedgerEvent ann] ->
  Validation
    (RegisterError ann)
    ( Maybe (RegisterBlock ann),
      -- Rest of the events, the ones that are not in the block.
      [LedgerEvent ann],
      -- New state
      RegisterState ann
    )
tallyBlock f showVirtual mBegin mEnd blockSize mCurrentBlock mCurrencyTo prices blockNum initialState events = do
  let initialRunningTotal = rsConvertedRunning initialState
  let makeEmptyBlock registerBlockTitle =
        let registerBlockEntries = V.empty
            registerBlockTotal = MultiAccount.zero
            registerBlockRunningTotal = initialRunningTotal
            registerBlockRunningAverage = computeAverage initialRunningTotal blockNum
         in RegisterBlock {..}
  let doEmptyBlock title = pure (Just (makeEmptyBlock title), events, initialState)
  -- We need to find the first event that produces an entry to make the title
  let goFirst :: RegisterState ann -> [LedgerEvent ann] -> Validation (RegisterError ann) (Maybe ((RegisterEntry ann, RegisterState ann), [LedgerEvent ann]))
      goFirst state = \case
        [] -> pure Nothing
        (evt : evts) -> do
          mEntry <-
            tallyEvent
              f
              showVirtual
              mBegin
              mEnd
              mCurrencyTo
              prices
              state
              evt
          case mEntry of
            Nothing -> goFirst state evts
            Just (entry, newState) -> pure $ Just ((entry, newState), evts)
  mFirst <- goFirst (initialState {rsConvertedBlockRunning = MultiAccount.zero}) events
  case mFirst of
    Nothing -> case mCurrentBlock of
      Just current -> doEmptyBlock current
      Nothing -> pure (Nothing, events, initialState)
    Just ((first, stateAfterFirst), tailEvents) -> do
      let firstTimestamp = registerEntryTimestamp first
          Located _ ts = firstTimestamp
      let registerBlockTitle = timestampBlockTitle ts blockSize
      let mEmpty = do
            currentBlock <- mCurrentBlock
            guard $ registerBlockTitle > currentBlock
            pure currentBlock
      case mEmpty of
        Just title -> doEmptyBlock title
        Nothing -> do
          let goEvents ::
                RegisterState ann ->
                [LedgerEvent ann] ->
                Validation
                  (RegisterError ann)
                  ( [RegisterEntry ann],
                    [LedgerEvent ann],
                    RegisterState ann
                  )
              goEvents state = \case
                [] -> pure ([], [], state)
                (evt : evts) -> do
                  let evtDay = eventDay evt
                  -- FIXME Comparing the title by text seems like a bad idea
                  -- TODO it's also wrong because individual blocks all have an empty title.
                  let title = dayBlockTitle evtDay blockSize
                  if title == registerBlockTitle
                    then do
                      mEntry <-
                        tallyEvent
                          f
                          showVirtual
                          mBegin
                          mEnd
                          mCurrencyTo
                          prices
                          state
                          evt
                      case mEntry of
                        Nothing -> goEvents state evts
                        Just (entry, newState) -> do
                          (entries, restEvents, finalState) <- goEvents newState evts
                          pure (entry : entries, restEvents, finalState)
                    else pure ([], evt : evts, state)

          (talliedEntries, outOfBlockEvents, finalState) <-
            goEvents stateAfterFirst tailEvents
          let registerBlockRunningTotal = rsConvertedRunning finalState
              registerBlockTotal = rsConvertedBlockRunning finalState
              registerBlockEntries = V.fromList (first : talliedEntries)
              -- Only count this block for averaging if it has transactions
              hasTransactions = V.any isTransactionEntry registerBlockEntries
              effectiveBlockNum = if hasTransactions then blockNum else max 1 (blockNum - 1)
              registerBlockRunningAverage = computeAverage registerBlockRunningTotal effectiveBlockNum
          pure (Just RegisterBlock {..}, outOfBlockEvents, finalState)

-- | Get the timestamp of a register entry
registerEntryTimestamp :: RegisterEntry ann -> GenLocated ann Timestamp
registerEntryTimestamp = \case
  RegisterEntryTransaction rt -> registerTransactionTimestamp rt
  RegisterEntryRevaluation rr -> registerRevaluationTimestamp rr

-- | Process a single ledger event (transaction or price) and produce a register entry if applicable
tallyEvent ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  RegisterState ann ->
  LedgerEvent ann ->
  Validation
    (RegisterError ann)
    (Maybe (RegisterEntry ann, RegisterState ann))
tallyEvent f showVirtual mBegin mEnd mCurrencyTo prices state = \case
  EventTransaction tx -> do
    mResult <- tallyTransaction f showVirtual mBegin mEnd mCurrencyTo prices state tx
    pure $ case mResult of
      Nothing -> Nothing
      Just (rt, newState) -> Just (RegisterEntryTransaction rt, newState)
  EventPrice lp -> do
    -- Only process price events when we have a conversion currency
    case mCurrencyTo of
      Nothing -> pure Nothing
      Just currencyTo ->
        tallyPriceEvent mBegin mEnd prices currencyTo state lp

-- | Process a price event to potentially create a revaluation entry
tallyPriceEvent ::
  forall ann.
  (Ord ann) =>
  Maybe Day ->
  Maybe Day ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Currency ann ->
  RegisterState ann ->
  GenLocated ann (Price ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RegisterEntry ann, RegisterState ann))
tallyPriceEvent mBegin mEnd prices currencyTo state (Located _ Price {..}) = do
  let Located _ ts = priceTimestamp
      day = Timestamp.toDay ts
  -- Check if this price is within the date filter
  if not (dayPassesDayFilter mBegin mEnd day)
    then pure Nothing
    else do
      let rawBalances = rsRawBalances state
      -- If we have no raw balances, no revaluation needed
      if rawBalances == MultiAccount.zero
        then pure Nothing
        else do
          -- Get the price graph BEFORE this price was added
          let oldPrices = case M.lookupLT day prices of
                Nothing -> MemoisedPriceGraph.empty
                Just (_, pg) -> pg
          -- Get the price graph AFTER this price (which includes it)
          let newPrices = case M.lookupLE day prices of
                Nothing -> MemoisedPriceGraph.empty
                Just (_, pg) -> pg
          -- Try to convert raw balances with old prices
          -- If conversion fails (missing price), treat old value as zero (first price establishment)
          let oldConvertedResult =
                mapValidationFailure RegisterErrorConvertError $
                  convertMultiAccount Nothing oldPrices currencyTo rawBalances
              oldConverted = case oldConvertedResult of
                Failure _ -> MultiAccount.zero -- No price available before, treat as zero
                Success v -> v
          -- Convert raw balances with new prices
          newConverted <-
            mapValidationFailure RegisterErrorConvertError $
              convertMultiAccount Nothing newPrices currencyTo rawBalances
          -- Calculate the difference (revaluation amount)
          case MultiAccount.subtract newConverted oldConverted of
            Nothing -> validationFailure RegisterErrorAddError
            Just revaluationAmount ->
              -- Only create a revaluation entry if there's a non-zero change
              if revaluationAmount == MultiAccount.zero
                then pure Nothing
                else do
                  -- Update the running totals
                  case MultiAccount.add (rsConvertedRunning state) revaluationAmount of
                    Nothing -> validationFailure RegisterErrorAddError
                    Just newRunning ->
                      case MultiAccount.add (rsConvertedBlockRunning state) revaluationAmount of
                        Nothing -> validationFailure RegisterErrorAddError
                        Just newBlockRunning -> do
                          let reval =
                                RegisterRevaluation
                                  { registerRevaluationTimestamp = priceTimestamp,
                                    registerRevaluationAmount = revaluationAmount,
                                    registerRevaluationRunningTotal = newRunning,
                                    registerRevaluationBlockRunningTotal = newBlockRunning
                                  }
                          let newState =
                                state
                                  { rsConvertedRunning = newRunning,
                                    rsConvertedBlockRunning = newBlockRunning
                                  }
                          pure $ Just (RegisterEntryRevaluation reval, newState)

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

tallyTransaction ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  RegisterState ann ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RegisterTransaction ann, RegisterState ann))
tallyTransaction
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
        let runningTotal = rsConvertedRunning state
            blockRunningTotal = rsConvertedBlockRunning state
            rawBalances = rsRawBalances state
        let goPostings running blockRunning rawBal = \case
              [] -> pure ([], running, blockRunning, rawBal)
              posting : rest -> do
                let Located _ ts = transactionTimestamp
                    day = Timestamp.toDay ts
                    dayPrices = case M.lookupLE day prices of
                      Nothing -> MemoisedPriceGraph.empty
                      Just (_, pg) -> pg
                mPosting <-
                  tallyPosting
                    f
                    showVirtual
                    mCurrencyTo
                    dayPrices
                    running
                    blockRunning
                    posting
                case mPosting of
                  Nothing -> goPostings running blockRunning rawBal rest
                  Just (blockPosting, rawDelta) -> do
                    -- Update raw balances with the unconverted amount
                    newRawBal <- case MultiAccount.add rawBal rawDelta of
                      Nothing -> validationFailure RegisterErrorAddError
                      Just r -> pure r
                    (ps, finalRunning, finalBlockRunning, finalRawBal) <-
                      goPostings
                        (registerPostingRunningTotal blockPosting)
                        (registerPostingBlockRunningTotal blockPosting)
                        newRawBal
                        rest
                    pure (blockPosting : ps, finalRunning, finalBlockRunning, finalRawBal)
        (talliedPostings, newRunningTotal, newBlockRunningTotal, newRawBalances) <-
          goPostings runningTotal blockRunningTotal rawBalances (V.toList transactionPostings)
        let registerTransactionTimestamp = transactionTimestamp
            registerTransactionDescription = transactionDescription
            registerTransactionPostings = V.fromList talliedPostings
        pure $
          if null talliedPostings
            then Nothing
            else
              Just
                ( RegisterTransaction {..},
                  RegisterState
                    { rsConvertedRunning = newRunningTotal,
                      rsConvertedBlockRunning = newBlockRunningTotal,
                      rsRawBalances = newRawBalances
                    }
                )
      else pure Nothing

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

tallyPosting ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe (Currency ann) ->
  MemoisedPriceGraph (Currency ann) ->
  Money.MultiAccount (Currency ann) ->
  Money.MultiAccount (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation
    (RegisterError ann)
    (Maybe (RegisterPosting ann, Money.MultiAccount (Currency ann)))
tallyPosting
  f
  showVirtual
  mCurrencyTo
  prices
  runningTotal
  blockRunning
  originalPosting@(Located _ origP) = do
    -- Get the raw (unconverted) amount from the original posting
    let Located _ origCurrency = postingCurrency origP
        Located _ origAccount = postingAccount origP
        rawAmount = MultiAccount.fromAccount origCurrency origAccount
    mPosting <- registerBlockPosting f showVirtual mCurrencyTo prices originalPosting
    case mPosting of
      Nothing -> pure Nothing
      Just registerPosting@(Located _ Posting {..}) -> do
        let Located _ currency = postingCurrency
        let Located _ account = postingAccount
        let ma = MultiAccount.fromAccount currency account

        case MultiAccount.add runningTotal ma of
          Nothing -> validationFailure RegisterErrorAddError -- TODO helpful location
          Just registerPostingRunningTotal -> case MultiAccount.add blockRunning ma of
            Nothing -> validationFailure RegisterErrorAddError -- TODO helpful location
            Just registerPostingBlockRunningTotal -> pure $ Just (RegisterPosting {..}, rawAmount)

registerBlockPosting ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe (Currency ann) ->
  MemoisedPriceGraph (Currency ann) ->
  GenLocated ann (Posting ann) ->
  Validation (RegisterError ann) (Maybe (GenLocated ann (Posting ann)))
registerBlockPosting f showVirtual mCurrencyTo prices lp@(Located pl p@Posting {..}) = do
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
