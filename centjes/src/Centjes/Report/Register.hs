{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Register
  ( Register (..),
    RegisterBlock (..),
    RegisterTransaction (..),
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
import Centjes.Validation
import Control.Monad
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
    registerBlockTransactions :: !(Vector (RegisterTransaction ann)),
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
        declare "The total of the block matches the sum of the postings" $
          (mapM registerTransactionTotal registerBlockTransactions >>= MultiAccount.sum)
            == Just registerBlockTotal
      ]

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
  let goBlocks ::
        Maybe Block ->
        Integer ->
        Money.MultiAccount (Currency ann) ->
        [GenLocated ann (Transaction ann)] ->
        Validation
          (RegisterError ann)
          ( [RegisterBlock ann],
            Money.MultiAccount (Currency ann)
          )
      goBlocks mCurrentBlock blockNum runningTotal ts = do
        (mBlock, restTransactions) <-
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
            runningTotal
            ts
        case mBlock of
          Nothing -> do
            let done = pure ([], runningTotal)
            case mEndBlock of
              Nothing -> done
              Just endBlock -> case mCurrentBlock of
                Nothing -> done
                Just currentBlock ->
                  if currentBlock >= endBlock
                    then done
                    else goBlocks (Just (nextBlock currentBlock)) blockNum runningTotal ts
          Just (block, newRunningTotal) -> do
            let currentBlock = registerBlockTitle block
            let done = pure ([block], newRunningTotal)
                continue = do
                  (restBlocks, total) <-
                    goBlocks
                      (Just (nextBlock currentBlock))
                      (succ blockNum)
                      newRunningTotal
                      restTransactions
                  pure (block : restBlocks, total)
             in case mEndBlock of
                  Nothing ->
                    if null (registerBlockTransactions block)
                      then pure ([], runningTotal)
                      else continue
                  Just endBlock ->
                    if currentBlock >= endBlock
                      then done
                      else continue
  (blocks, registerTotal) <-
    goBlocks
      mBeginBlock
      1
      MultiAccount.zero
      (V.toList (ledgerTransactions ledger))
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
  Money.MultiAccount (Currency ann) ->
  [GenLocated ann (Transaction ann)] ->
  Validation
    (RegisterError ann)
    ( Maybe
        ( RegisterBlock ann,
          Money.MultiAccount (Currency ann)
        ),
      -- Rest of the transactions, the ones that are not in the block.
      [GenLocated ann (Transaction ann)]
    )
tallyBlock f showVirtual mBegin mEnd blockSize mCurrentBlock mCurrencyTo prices blockNum initialRunningTotal transactions = do
  let makeEmptyBlock registerBlockTitle =
        let registerBlockTransactions = V.empty
            registerBlockTotal = MultiAccount.zero
            registerBlockRunningTotal = initialRunningTotal
            registerBlockRunningAverage = computeAverage initialRunningTotal blockNum
         in RegisterBlock {..}
  let doEmptyBlock title = pure (Just (makeEmptyBlock title, initialRunningTotal), transactions)
  -- We need to find the first transaction that's not filtered out to make the title
  let goFirst = \case
        [] -> pure Nothing
        (t : ts) -> do
          mTransaction <-
            tallyTransaction
              f
              showVirtual
              mBegin
              mEnd
              mCurrencyTo
              prices
              initialRunningTotal
              MultiAccount.zero
              t
          case mTransaction of
            Nothing -> goFirst ts
            Just tup -> pure $ Just (tup, ts)
  mFirst <- goFirst transactions
  case mFirst of
    Nothing -> case mCurrentBlock of
      Just current -> doEmptyBlock current
      Nothing -> pure (Nothing, transactions)
    Just ((first, runningTotal, blockRunningTotal), tailTransactions) -> do
      let Located _ firstTimestamp = registerTransactionTimestamp first
      let registerBlockTitle = timestampBlockTitle firstTimestamp blockSize
      let mEmpty = do
            currentBlock <- mCurrentBlock
            guard $ registerBlockTitle > currentBlock
            pure currentBlock
      case mEmpty of
        Just title -> doEmptyBlock title
        Nothing -> do
          let goTransactions ::
                Money.MultiAccount (Currency ann) ->
                Money.MultiAccount (Currency ann) ->
                [GenLocated ann (Transaction ann)] ->
                Validation
                  (RegisterError ann)
                  ( ( [RegisterTransaction ann],
                      Money.MultiAccount (Currency ann),
                      Money.MultiAccount (Currency ann)
                    ),
                    [GenLocated ann (Transaction ann)]
                  )
              goTransactions running blockRunning = \case
                [] -> pure (([], running, blockRunning), [])
                (lt : lts) -> do
                  let Located _ Transaction {..} = lt
                      Located _ ts = transactionTimestamp
                  -- FIXME Comparing the title by text seems like a bad idea
                  -- TODO it's also wrong because individual blocks all have an empty title.
                  let title = timestampBlockTitle ts blockSize
                  if title == registerBlockTitle
                    then do
                      mTransaction <-
                        tallyTransaction
                          f
                          showVirtual
                          mBegin
                          mEnd
                          mCurrencyTo
                          prices
                          running
                          blockRunning
                          lt
                      case mTransaction of
                        Nothing -> goTransactions running blockRunning lts
                        Just (tr, newRunning, newBlockRunning) -> do
                          ((trs, total, blockTotal), restTransactions) <- goTransactions newRunning newBlockRunning lts
                          pure ((tr : trs, total, blockTotal), restTransactions)
                    else pure (([], running, blockRunning), lt : lts)

          ((talliedTransactions, registerBlockRunningTotal, registerBlockTotal), outOfBlockTransactions) <-
            goTransactions
              runningTotal
              blockRunningTotal
              tailTransactions
          let registerBlockRunningAverage = computeAverage registerBlockRunningTotal blockNum
          let registerBlockTransactions = V.fromList (first : talliedTransactions)
          pure (Just (RegisterBlock {..}, registerBlockRunningTotal), outOfBlockTransactions)

tallyTransaction ::
  (Ord ann) =>
  Filter ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Maybe (Currency ann) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Money.MultiAccount (Currency ann) ->
  Money.MultiAccount (Currency ann) ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    ( Maybe
        ( RegisterTransaction ann,
          Money.MultiAccount (Currency ann),
          Money.MultiAccount (Currency ann)
        )
    )
tallyTransaction
  f
  showVirtual
  mBegin
  mEnd
  mCurrencyTo
  prices
  runningTotal
  blockRunningTotal
  (Located _ Transaction {..}) =
    if timestampPassesDayFilter mBegin mEnd transactionTimestamp
      then do
        let goPostings running blockRunning = \case
              [] -> pure ([], running, blockRunning)
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
                  Nothing -> goPostings running blockRunning rest
                  Just blockPosting -> do
                    (ps, finalRunning, finalBlockRunning) <-
                      goPostings
                        (registerPostingRunningTotal blockPosting)
                        (registerPostingBlockRunningTotal blockPosting)
                        rest
                    pure (blockPosting : ps, finalRunning, finalBlockRunning)
        (talliedPostings, newRunningTotal, newBlockRunningTotal) <-
          goPostings runningTotal blockRunningTotal (V.toList transactionPostings)
        let registerTransactionTimestamp = transactionTimestamp
            registerTransactionDescription = transactionDescription
            registerTransactionPostings = V.fromList talliedPostings
        pure $
          if null talliedPostings
            then Nothing
            else
              Just
                ( RegisterTransaction {..},
                  newRunningTotal,
                  newBlockRunningTotal
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
    (Maybe (RegisterPosting ann))
tallyPosting
  f
  showVirtual
  mCurrencyTo
  prices
  runningTotal
  blockRunning
  originalPosting = do
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
            Just registerPostingBlockRunningTotal -> pure $ Just RegisterPosting {..}

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
