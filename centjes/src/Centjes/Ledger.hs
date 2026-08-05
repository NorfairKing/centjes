{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Centjes.Ledger
  ( Ledger (..),
    Account (..),
    VirtualPostingPolicy (..),
    Timestamp (..),
    CurrencySymbol (..),
    Price (..),
    Transaction (..),
    Assertion (..),
    AssertionScope (..),
    Description (..),
    Posting (..),
    PostingPrice (..),
    postingCommodity,
    postingPriceCommodity,
    postingCurrency,
    postingConversion,
    Cost (..),
    AmountRatio (..),
    Attachment (..),
    Tag (..),
    Commodity (..),
    commodityCurrency,
    commodityQuantisationFactor,
    commodityText,
    Currency (..),
    Lot (..),
    lotCost,
    lotText,
    AccountName (..),
    AccountType (..),
  )
where

import Centjes.AccountName (AccountName (..))
import Centjes.AccountType (AccountType (..))
import Centjes.Location
import Centjes.Module (AssertionScope (..), Attachment (..), CurrencySymbol (..), Description (..))
import Centjes.Tag
import Centjes.Timestamp as Timestamp
import Data.Function
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Ratio (denominator, numerator)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
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
import qualified Money.ConversionRate as ConversionRate
import Money.QuantisationFactor
import qualified Numeric.DecimalLiteral as DecimalLiteral

{-# ANN module ("DisableMutations" :: String) #-}

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
        currencyDeclared currency =
          declare "The currency is in the currencies map" $
            currency `S.member` currenciesSet
        commodityDeclared = \case
          CommodityCurrency currency -> currencyDeclared currency
          CommodityLot Lot {..} ->
            mconcat
              [ currencyDeclared lotCurrency,
                currencyDeclared lotBasisCurrency
              ]
        costCurrencyValid (Located _ Cost {..}) =
          let Cost _ _ = undefined
           in let Located _ currency = costCurrency
               in currencyDeclared currency
     in mconcat
          [ genericValidate l,
            decorateList (V.toList ledgerPrices) $ \(Located _ Price {..}) ->
              let Price _ _ _ = undefined
               in mconcat
                    [ let Located _ commodity = priceCommodity
                       in commodityDeclared commodity,
                      costCurrencyValid priceCost
                    ],
            decorateList (V.toList ledgerTransactions) $ \(Located _ Transaction {..}) ->
              let Transaction _ _ _ _ _ _ = undefined
               in mconcat
                    [ decorateList (V.toList transactionPostings) $ \(Located _ p@Posting {..}) ->
                        let Posting _ _ _ _ _ = undefined
                         in mconcat
                              [ declare "The posting's account name is in the accounts map" $
                                  let Located _ accountName = postingAccountName
                                   in accountName `S.member` accountsSet,
                                let Located _ commodity = postingCommodity p
                                 in commodityDeclared commodity,
                                case postingPrice of
                                  PostingPriceLot _ -> valid
                                  PostingPriceCurrency _ mCost -> foldMap costCurrencyValid mCost
                              ],
                      decorateMap transactionTags $ \tag _ ->
                        declare "The tag is in the tags map" $
                          tag `S.member` tagsSet,
                      decorateList (V.toList transactionAssertions) $ \(Located _ assertion) ->
                        case assertion of
                          AssertionEquals _ (Located _ accountName) _ (Located _ commodity) ->
                            mconcat
                              [ declare "The posting's account name is in the accounts map" $
                                  accountName `S.member` accountsSet,
                                commodityDeclared commodity
                              ]
                    ],
            declare "the prices are sorted" $
              partiallyOrderedByTimestamp priceTimestamp ledgerPrices,
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
    accountVirtualPostingPolicy :: !VirtualPostingPolicy,
    accountTags :: !(Map Tag ann)
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Account ann)

data VirtualPostingPolicy
  = VirtualPostingPolicyForbidden
  | VirtualPostingPolicyAllowed
  | VirtualPostingPolicyOnly
  deriving stock (Show, Eq, Generic)

instance Validity VirtualPostingPolicy

data Price ann = Price
  { priceTimestamp :: !(GenLocated ann Timestamp),
    -- Note: This field will have the source location of the currency _symbol_ in the price declaration.
    priceCommodity :: !(GenLocated ann (Commodity ann)),
    -- Note: This field will have the source declaration of the cost wherever it was declared.
    priceCost :: !(GenLocated ann (Cost ann))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Eq ann) => Validity (Price ann) where
  validate p@Price {..} =
    mconcat
      [ genericValidate p,
        declare "The price converts between two different commodities" $
          let Located _ from = priceCommodity
              Located _ Cost {..} = priceCost
              Located _ to = costCurrency
           in from /= CommodityCurrency to
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
      !AssertionScope
      !(GenLocated ann AccountName)
      -- Note: This field will have the source location of the decimal literal that defined it.
      !(GenLocated ann Money.Account)
      -- Note: This field will have the source location of the currency _symbol_ that defined it
      --
      -- A 'CommodityCurrency' asserts the total across every lot of the symbol;
      -- a 'CommodityLot' asserts that one lot.
      !(GenLocated ann (Commodity ann))
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Assertion ann)

data Posting ann = Posting
  { postingReal :: !Bool,
    postingAccountName :: !(GenLocated ann AccountName),
    -- Note: This field will have the source location of the decimal literal
    -- that defined it.
    postingAccount :: !(GenLocated ann Money.Account),
    postingPrice :: !(PostingPrice ann),
    -- Note: This field will have the source location of the amountRatio
    -- expression that defined it.
    postingAmountRatio :: !(Maybe (GenLocated ann (AmountRatio ann)))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Eq ann) => Validity (Posting ann)

-- | What a posting is denominated in, and what it converts at to balance.
--
-- A lot already says what the posting converts at, so the lot alternative has
-- nowhere to put a cost as well: a second rate could disagree with the first.
data PostingPrice ann
  = -- | @5 USD@ and @5 USD \@ 1 EUR@
    --
    -- Note: The located fields will have the source location of the currency
    -- _symbol_ that defined them.
    PostingPriceCurrency !(GenLocated ann (Currency ann)) !(Maybe (GenLocated ann (Cost ann)))
  | -- | @2 SWDA lot \@ 500 EUR@
    PostingPriceLot !(GenLocated ann (Lot ann))
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Eq ann) => Validity (PostingPrice ann) where
  validate pp =
    mconcat
      [ genericValidate pp,
        declare "The posting cost converts to a different currency" $
          case pp of
            PostingPriceLot _ -> True
            PostingPriceCurrency _ Nothing -> True
            PostingPriceCurrency (Located _ from) (Just (Located _ Cost {..})) ->
              let Located _ to = costCurrency
               in from /= to
      ]

-- | What a posting is denominated in.
postingCommodity :: Posting ann -> GenLocated ann (Commodity ann)
postingCommodity = postingPriceCommodity . postingPrice

postingPriceCommodity :: PostingPrice ann -> GenLocated ann (Commodity ann)
postingPriceCommodity = \case
  PostingPriceCurrency (Located l currency) _ -> Located l (CommodityCurrency currency)
  PostingPriceLot (Located l lot) -> Located l (CommodityLot lot)

-- | What a posting is denominated in, disregarding any lot.
--
-- For a report that has one line per currency rather than one per commodity.
postingCurrency :: Posting ann -> Currency ann
postingCurrency = commodityCurrency . locatedValue . postingCommodity

-- | The conversion a posting balances with, if it converts at all.
--
-- A lot converts at the rate it was acquired at, which is the same conversion
-- the cost annotation it replaces would have produced.
postingConversion :: Posting ann -> Maybe (GenLocated ann (Cost ann))
postingConversion p = case postingPrice p of
  PostingPriceCurrency _ mCost -> mCost
  PostingPriceLot (Located l lot) -> Just (Located l (lotCost l lot))

data Cost ann = Cost
  { -- Note: This field will have the source location of the decimal literal in the cost
    costConversionRate :: !(GenLocated ann ConversionRate),
    -- Note: This field will have the source location of the currency _symbol_ in the cost
    costCurrency :: !(GenLocated ann (Currency ann))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Cost ann)

data AmountRatio ann = AmountRatio
  { amountRatioInclusive :: !Bool,
    amountRatioRounding :: !Rounding,
    -- Note: This field will have the source location of the decimal literal in
    -- the amountRatio expression
    amountRatio :: !(GenLocated ann Rational)
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (AmountRatio ann)

-- | Anything that a balance can be denominated in.
--
-- A lot is a commodity of its own: it never shares a balance with the currency
-- it is a lot of, nor with a lot of that currency at another price.
data Commodity ann
  = CommodityCurrency !(Currency ann)
  | CommodityLot !(Lot ann)
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity ann) => Validity (Commodity ann)

-- | The currency that a commodity is denominated in.
commodityCurrency :: Commodity ann -> Currency ann
commodityCurrency = \case
  CommodityCurrency currency -> currency
  CommodityLot lot -> lotCurrency lot

commodityQuantisationFactor :: Commodity ann -> GenLocated ann QuantisationFactor
commodityQuantisationFactor = currencyQuantisationFactor . commodityCurrency

-- | Render a commodity the way it is written in a posting.
commodityText :: Commodity ann -> Text
commodityText = \case
  CommodityCurrency currency -> currencySymbolText (currencySymbol currency)
  CommodityLot lot -> lotText lot

data Currency ann = Currency
  { currencySymbol :: !CurrencySymbol,
    -- Note: This field will have the source location of currency _declaration_ that defined it
    currencyQuantisationFactor :: !(GenLocated ann QuantisationFactor)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity ann) => Validity (Currency ann)

-- | A currency acquired at a known price per unit.
--
-- This is part of the identity of a commodity, so it holds no source locations
-- other than declaration locations: two mentions of the same lot in different
-- places have to compare equal, or their balances would not add up. Any field
-- added here has to keep that property.
--
-- Holding a 'Currency' rather than a 'Commodity' is what makes a lot of a lot
-- unrepresentable.
data Lot ann = Lot
  { lotCurrency :: !(Currency ann),
    lotBasisRate :: !ConversionRate,
    lotBasisCurrency :: !(Currency ann)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (Validity ann) => Validity (Lot ann) where
  validate l@Lot {..} =
    mconcat
      [ genericValidate l,
        declare "The lot is priced in a different currency than it is a lot of" $
          currencySymbol lotBasisCurrency /= currencySymbol lotCurrency
      ]

-- | What one unit of a lot cost, as a cost at the given source location.
--
-- This is the conversion that a lot posting balances with.
lotCost :: ann -> Lot ann -> Cost ann
lotCost l Lot {..} =
  Cost
    { costConversionRate = Located l lotBasisRate,
      costCurrency = Located l lotBasisCurrency
    }

-- | Render a lot the way it is written in a posting.
lotText :: Lot ann -> Text
lotText Lot {..} =
  T.pack $
    unwords
      [ T.unpack (currencySymbolText (currencySymbol lotCurrency)),
        "lot",
        "@",
        case ConversionRate.toDecimalLiteral lotBasisRate of
          Just dl -> DecimalLiteral.toString dl
          Nothing ->
            let r = ConversionRate.toRatio lotBasisRate
             in unwords [show (numerator r), "/", show (denominator r)],
        T.unpack (currencySymbolText (currencySymbol lotBasisCurrency))
      ]
