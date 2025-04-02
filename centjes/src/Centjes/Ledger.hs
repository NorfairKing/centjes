{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Centjes.Ledger
  ( Ledger (..),
    Account (..),
    Timestamp (..),
    CurrencySymbol (..),
    Price (..),
    Transaction (..),
    Assertion (..),
    Description (..),
    Posting (..),
    Cost (..),
    Percentage (..),
    Attachment (..),
    Tag (..),
    Currency (..),
    AccountName (..),
    AccountType (..),
  )
where

import Centjes.AccountName (AccountName (..))
import Centjes.AccountType (AccountType (..))
import Centjes.Location
import Centjes.Module (Attachment (..), CurrencySymbol (..), Description (..))
import Centjes.Tag
import Centjes.Timestamp as Timestamp
import Data.Function
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Map
import Data.Validity.Set ()
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.Amount (Rounding (..))
import Money.ConversionRate (ConversionRate)
import Money.QuantisationFactor
import Numeric.Natural

data Ledger ann = Ledger
  { -- Note: This field will have the source location of the currency _declaration_ that defined it.
    ledgerCurrencies :: !(Map CurrencySymbol (GenLocated ann QuantisationFactor)),
    -- Note: This field will have the source location of the account _ declaration_ that defined it.
    ledgerAccounts :: !(Map AccountName (GenLocated ann (Account ann))),
    -- Note: This field will have the source location of the tag _declaration_ that defined it
    ledgerTags :: !(Map Tag ann),
    ledgerPrices :: !(Vector (GenLocated ann (Price ann))),
    ledgerTransactions :: !(Vector (GenLocated ann (Transaction ann)))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Ledger ann) where
  validate l@(Ledger {..}) =
    let currenciesSet = S.fromList $ map (uncurry Currency) $ M.toList ledgerCurrencies
        accountsSet = M.keysSet ledgerAccounts
        tagsSet = M.keysSet ledgerTags
        costCurrencyValid (Located _ Cost {..}) =
          let Cost _ _ = undefined
           in declare "The cost's currency is in the currencies map" $
                let Located _ currency = costCurrency
                 in currency `S.member` currenciesSet
     in mconcat
          [ genericValidate l,
            decorateList (V.toList ledgerPrices) $ \(Located _ Price {..}) ->
              let Price _ _ _ = undefined
               in mconcat
                    [ declare "The price's currency is in the currencies set" $
                        let Located _ currency = priceCurrency
                         in currency `S.member` currenciesSet,
                      costCurrencyValid priceCost
                    ],
            decorateList (V.toList ledgerTransactions) $ \(Located _ Transaction {..}) ->
              let Transaction _ _ _ _ _ _ = undefined
               in mconcat
                    [ decorateList (V.toList transactionPostings) $ \(Located _ Posting {..}) ->
                        let Posting _ _ _ _ _ _ = undefined
                         in mconcat
                              [ declare "The posting's account name is in the accounts map" $
                                  let Located _ accountName = postingAccountName
                                   in accountName `S.member` accountsSet,
                                declare "The posting's currency is in the currencies map" $
                                  let Located _ currency = postingCurrency
                                   in currency `S.member` currenciesSet,
                                case postingCost of
                                  Nothing -> valid
                                  Just lc -> costCurrencyValid lc
                              ],
                      decorateMap transactionTags $ \tag _ ->
                        declare "The tag is in the tags map" $
                          tag `S.member` tagsSet,
                      decorateList (V.toList transactionAssertions) $ \(Located _ assertion) ->
                        case assertion of
                          AssertionEquals (Located _ accountName) _ (Located _ currency) ->
                            mconcat
                              [ declare "The posting's account name is in the accounts map" $
                                  accountName `S.member` accountsSet,
                                declare "The assertion's currency is in the currency map" $
                                  S.member currency currenciesSet
                              ]
                    ],
            declare "the prices are sorted" $ partiallyOrderedByTimestamp priceTimestamp ledgerPrices,
            declare "the transactions are sorted" $
              partiallyOrderedByTimestamp transactionTimestamp ledgerTransactions
          ]

partiallyOrderedByTimestamp :: (a -> GenLocated ann Timestamp) -> Vector (GenLocated ann a) -> Bool
partiallyOrderedByTimestamp getTimestamp =
  partiallyOrderedBy
    (Timestamp.comparePartially `on` (locatedValue . getTimestamp . locatedValue))

partiallyOrderedBy :: (a -> a -> Maybe Ordering) -> Vector a -> Bool
partiallyOrderedBy f v =
  if V.null v
    then True
    else V.and (V.zipWith (\a1 a2 -> f a1 a2 /= Just GT) v (V.tail v))

data Account ann = Account
  { accountType :: !AccountType,
    accountAttachments :: !(Vector (GenLocated ann (Attachment ann))),
    -- | Which currencies are allowed in this account
    --
    -- Nothing means "any"
    -- Just S.empty "none"
    accountCurrencies :: !(Maybe (Set (Currency ann))),
    accountTags :: !(Map Tag ann)
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Account ann)

data Price ann = Price
  { priceTimestamp :: !(GenLocated ann Timestamp),
    -- Note: This field will have the source location of the currency _symbol_ in the price declaration.
    priceCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source declaration of the cost wherever it was declared.
    priceCost :: !(GenLocated ann (Cost ann))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Eq ann) => Validity (Price ann) where
  validate p@Price {..} =
    mconcat
      [ genericValidate p,
        declare "The price cost has a different currency" $
          let Located _ pCur = priceCurrency
              Located _ Cost {..} = priceCost
              Located _ cCur = costCurrency
           in pCur /= cCur
      ]

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: !(Vector (GenLocated ann (Posting ann))),
    transactionAttachments :: !(Vector (GenLocated ann (Attachment ann))),
    transactionAssertions :: !(Vector (GenLocated ann (Assertion ann))),
    -- Note: This field will have the source location of the tag "extra" syntax element
    transactionTags :: !(Map Tag ann)
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Transaction ann)

data Assertion ann
  = AssertionEquals
      !(GenLocated ann AccountName)
      -- Note: This field will have the source location of the decimal literal that defined it.
      !(GenLocated ann Money.Account)
      -- Note: This field will have the source location of the currency _symbol_ that defined it
      !(GenLocated ann (Currency ann))
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Assertion ann)

data Posting ann = Posting
  { postingReal :: !Bool,
    postingAccountName :: !(GenLocated ann AccountName),
    -- Note: This field will have the source location of the currency _symbol_
    -- that defined it.
    postingCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source location of the decimal literal
    -- that defined it.
    postingAccount :: !(GenLocated ann Money.Account),
    postingCost :: !(Maybe (GenLocated ann (Cost ann))),
    -- Note: This field will have the source location of the percentage
    -- expression that defined it.
    postingPercentage :: !(Maybe (GenLocated ann (Percentage ann)))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Eq ann) => Validity (Posting ann) where
  validate p@Posting {..} =
    mconcat
      [ genericValidate p,
        declare "The posting cost has a different currency" $
          case postingCost of
            Nothing -> True
            Just (Located _ Cost {..}) ->
              let Located _ pCur = postingCurrency
                  Located _ cCur = costCurrency
               in pCur /= cCur
      ]

data Cost ann = Cost
  { -- Note: This field will have the source location of the decimal literal in the cost
    costConversionRate :: !(GenLocated ann ConversionRate),
    -- Note: This field will have the source location of the currency _symbol_ in the cost
    costCurrency :: !(GenLocated ann (Currency ann))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Cost ann)

data Percentage ann = Percentage
  { percentageInclusive :: !Bool,
    percentageRounding :: !Rounding,
    -- Note: This field will have the source location of the decimal literal in
    -- the percentage expression
    -- Note: This field does not contain a percentage anymore. I.e. the /100
    -- has already been applied. It is just called this because of what it's
    -- called in the module. TODO maybe we want to rename it?
    percentageRatio :: !(GenLocated ann (Ratio Natural))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Percentage ann)

data Currency ann = Currency
  { currencySymbol :: !CurrencySymbol,
    -- Note: This field will have the source location of currency _declaration_ that defined it
    currencyQuantisationFactor :: !(GenLocated ann QuantisationFactor)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity ann) => Validity (Currency ann)
