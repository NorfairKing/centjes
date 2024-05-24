{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatCurrencyDeclaration,
    formatAccountDeclaration,
    formatTagDeclaration,
    formatPriceDeclaration,
    formatTransaction,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.AccountType as AccountType
import Centjes.Location
import Centjes.Module
import qualified Centjes.Tag as Tag
import qualified Centjes.Timestamp as Timestamp
import Data.List (intersperse)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Numeric.DecimalLiteral as DecimalLiteral
import Path
import Prettyprinter
import Prettyprinter.Render.Text

formatModule :: Module l -> Text
formatModule = renderDocText . moduleDoc

formatDeclaration :: Declaration l -> Text
formatDeclaration = renderDocText . declarationDoc

formatCurrencyDeclaration :: CurrencyDeclaration l -> Text
formatCurrencyDeclaration = renderDocText . currencyDeclarationDoc

formatAccountDeclaration :: AccountDeclaration l -> Text
formatAccountDeclaration = renderDocText . accountDeclarationDoc

formatTagDeclaration :: TagDeclaration l -> Text
formatTagDeclaration = renderDocText . tagDeclarationDoc

formatPriceDeclaration :: PriceDeclaration l -> Text
formatPriceDeclaration = renderDocText . priceDeclarationDoc

formatTransaction :: Transaction l -> Text
formatTransaction = renderDocText . transactionDoc

renderDocText :: Doc ann -> Text
renderDocText = renderStrict . layoutPretty layoutOptions
  where
    layoutOptions = LayoutOptions {layoutPageWidth = Unbounded}

moduleDoc :: Module l -> Doc ann
moduleDoc Module {..} =
  mconcat $
    concat
      [ map lImportDoc moduleImports,
        [hardline | not (null moduleImports) && not (null moduleDeclarations)],
        go Nothing moduleDeclarations
      ]
  where
    go :: Maybe DecType -> [GenLocated l (Declaration l)] -> [Doc ann]
    go _ [] = []
    go Nothing (Located _ d : ds) =
      -- No newline up front, but remember the declaration type
      declarationDoc d <> hardline : go (Just (decType d)) ds
    go (Just dt) (Located _ d : ds) =
      let dt' = decType d
          insertEmptyLine = case (dt, dt') of
            -- Group comments together
            (DecComment, DecComment) -> False
            -- Attach comments to the next declaration
            (DecComment, _) -> False
            -- Group currency declarations together
            (DecCurrency, DecCurrency) -> False
            -- Group account declarations together
            (DecAccount, DecAccount) -> False
            -- Group tag declarations together
            (DecTag, DecTag) -> False
            -- Group price declarations together
            (DecPrice, DecPrice) -> False
            -- Don't group transactions together
            (DecTransaction, DecTransaction) -> True
            _ -> dt /= dt'
       in ( if insertEmptyLine
              then (hardline :)
              else id
          )
            $ declarationDoc d <> hardline : go (Just dt') ds

data DecType
  = DecComment
  | DecCurrency
  | DecAccount
  | DecTag
  | DecPrice
  | DecTransaction
  deriving (Eq)

decType :: Declaration l -> DecType
decType = \case
  DeclarationComment {} -> DecComment
  DeclarationCurrency {} -> DecCurrency
  DeclarationAccount {} -> DecAccount
  DeclarationTag {} -> DecTag
  DeclarationPrice {} -> DecPrice
  DeclarationTransaction {} -> DecTransaction

lImportDoc :: GenLocated l (Import l) -> Doc ann
lImportDoc = importDoc . locatedValue

importDoc :: Import l -> Doc ann
importDoc (Import (Located _ fp)) =
  let pString = fromRelFile $ case splitExtension fp of
        Just (rest, ".cent") -> rest
        _ -> fp
   in "import" <+> pretty pString <> hardline

declarationDoc :: Declaration l -> Doc ann
declarationDoc = \case
  DeclarationComment t -> commentDoc t
  DeclarationCurrency cd -> lCurrencyDeclarationDoc cd
  DeclarationAccount ad -> lAccountDeclarationDoc ad
  DeclarationTag ad -> lTagDeclarationDoc ad
  DeclarationPrice pd -> lPriceDeclarationDoc pd
  DeclarationTransaction t -> transactionDecDoc t

commentDoc :: GenLocated l Text -> Doc ann
commentDoc (Located _ t) =
  let ls = if T.null t then [""] else T.lines t
      commentLine l = "--" <+> pretty l
   in mconcat $ map commentLine ls

lCurrencyDeclarationDoc :: GenLocated l (CurrencyDeclaration l) -> Doc ann
lCurrencyDeclarationDoc = currencyDeclarationDoc . locatedValue

currencyDeclarationDoc :: CurrencyDeclaration l -> Doc ann
currencyDeclarationDoc CurrencyDeclaration {..} =
  "currency"
    <+> currencySymbolDoc (locatedValue currencyDeclarationSymbol)
    <+> quantisationFactorDoc currencyDeclarationQuantisationFactor

quantisationFactorDoc :: GenLocated l DecimalLiteral -> Doc ann
quantisationFactorDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

lAccountDeclarationDoc :: GenLocated l (AccountDeclaration l) -> Doc ann
lAccountDeclarationDoc = accountDeclarationDoc . locatedValue

accountDeclarationDoc :: AccountDeclaration l -> Doc ann
accountDeclarationDoc AccountDeclaration {..} =
  maybe
    id
    (\at -> (<+> lAccountTypeDoc at))
    accountDeclarationType
    ( "account"
        <+> lAccountNameDoc accountDeclarationName
    )

lAccountTypeDoc :: GenLocated l AccountType -> Doc ann
lAccountTypeDoc (Located _ at) = pretty $ AccountType.toText at

lTagDeclarationDoc :: GenLocated l (TagDeclaration l) -> Doc ann
lTagDeclarationDoc = tagDeclarationDoc . locatedValue

tagDeclarationDoc :: TagDeclaration l -> Doc ann
tagDeclarationDoc TagDeclaration {..} =
  "tag"
    <+> tagDoc (locatedValue tagDeclarationTag)

lPriceDeclarationDoc :: GenLocated l (PriceDeclaration l) -> Doc ann
lPriceDeclarationDoc = priceDeclarationDoc . locatedValue

priceDeclarationDoc :: PriceDeclaration l -> Doc ann
priceDeclarationDoc PriceDeclaration {..} =
  "price"
    <+> lTimestampDoc priceDeclarationTimestamp
    <+> lCurrencySymbolDoc priceDeclarationCurrencySymbol
    <+> lCostExpressionDoc priceDeclarationCost

lConversionRateDoc :: GenLocated l (RationalExpression l) -> Doc ann
lConversionRateDoc = conversionRateDoc . locatedValue

conversionRateDoc :: RationalExpression l -> Doc ann
conversionRateDoc = rationalExpressionDoc

transactionDecDoc :: GenLocated l (Transaction l) -> Doc ann
transactionDecDoc = transactionDoc . locatedValue

transactionDoc :: Transaction l -> Doc ann
transactionDoc Transaction {..} =
  mconcat $
    intersperse hardline $
      concat
        [ [lTimestampDoc transactionTimestamp],
          map ("  " <>) $ maybe [] descriptionDocs transactionDescription,
          map
            ( ("  " <>)
                . postingDocHelper
                  (Just maxAccountNameWidth)
                  (Just maxAccountWidth)
                  (Just maxAccountDecimals)
                . locatedValue
            )
            transactionPostings,
          map (("  " <>) . transactionExtraDoc . locatedValue) transactionExtras
        ]
  where
    maxAccountNameWidth = foldMap (accountNameWidth . postingAccountName . locatedValue) transactionPostings
    accountNameWidth = Max . T.length . AccountName.toText . locatedValue
    maxAccountWidth = foldMap (accountWidth . postingAccount . locatedValue) transactionPostings
    accountWidth = Max . charactersBeforeDot . locatedValue
    maxAccountDecimals = foldMap (accountDecimals . postingAccount . locatedValue) transactionPostings
    accountDecimals = Max . DecimalLiteral.digits . locatedValue

lTimestampDoc :: GenLocated l Timestamp -> Doc ann
lTimestampDoc = timestampDoc . locatedValue

timestampDoc :: Timestamp -> Doc ann
timestampDoc = pretty . Timestamp.toString

descriptionDocs :: GenLocated l Description -> [Doc ann]
descriptionDocs = map (pretty . ("| " <>)) . T.lines . unDescription . locatedValue

postingDocHelper :: Maybe (Max Int) -> Maybe (Max Int) -> Maybe (Max Word8) -> Posting l -> Doc ann
postingDocHelper mMaxAccountNameWidth mMaxAccountWidth mMaxAccountDecimals Posting {..} =
  maybe id (\pe -> (<+> ("~" <+> lPercentageExpressionDoc pe))) postingPercentage $
    maybe id (\ce -> (<+> ("@" <+> lCostExpressionDoc ce))) postingCost $
      (if postingReal then "*" else "!")
        <+> maybe id (fill . getMax) mMaxAccountNameWidth (lAccountNameDoc postingAccountName)
        <+> accountDocHelper mMaxAccountWidth mMaxAccountDecimals (locatedValue postingAccount)
        <+> lCurrencySymbolDoc postingCurrencySymbol

lCostExpressionDoc :: GenLocated l (CostExpression l) -> Doc ann
lCostExpressionDoc (Located _ CostExpression {..}) =
  lConversionRateDoc costExpressionConversionRate
    <+> lCurrencySymbolDoc costExpressionCurrencySymbol

lPercentageExpressionDoc :: GenLocated l (PercentageExpression l) -> Doc ann
lPercentageExpressionDoc (Located _ PercentageExpression {..}) =
  lRationalExpressionDoc unPercentageExpression
    <> "%"

lRationalExpressionDoc :: GenLocated l (RationalExpression l) -> Doc ann
lRationalExpressionDoc = rationalExpressionDoc . locatedValue

rationalExpressionDoc :: RationalExpression l -> Doc ann
rationalExpressionDoc = \case
  RationalExpressionDecimal ldl -> lRationalDecimalLiteralDoc ldl
  RationalExpressionFraction ln ld ->
    lRationalDecimalLiteralDoc ln
      <+> "/"
      <+> lRationalDecimalLiteralDoc ld

lRationalDecimalLiteralDoc :: GenLocated l DecimalLiteral -> Doc ann
lRationalDecimalLiteralDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

transactionExtraDoc :: TransactionExtra l -> Doc ann
transactionExtraDoc =
  ("+" <+>) . \case
    TransactionAttachment a -> extraAttachmentDoc (locatedValue a)
    TransactionAssertion a -> extraAssertionDoc (locatedValue a)
    TransactionTag t -> extraTagDoc (locatedValue t)

extraAttachmentDoc :: ExtraAttachment l -> Doc ann
extraAttachmentDoc (ExtraAttachment (Located _ (Attachment (Located _ fp)))) =
  "attach"
    <+> pretty (fromRelFile fp)

extraAssertionDoc :: ExtraAssertion l -> Doc ann
extraAssertionDoc (ExtraAssertion (Located _ (AssertionEquals an (Located _ dl) cs))) =
  "assert"
    <+> lAccountNameDoc an
    <+> "="
    <+> accountDoc dl
    <+> lCurrencySymbolDoc cs

extraTagDoc :: ExtraTag l -> Doc ann
extraTagDoc (ExtraTag lt) =
  "tag" <+> tagDoc (locatedValue lt)

tagDoc :: Tag -> Doc ann
tagDoc = pretty . Tag.toText

lAccountNameDoc :: GenLocated l AccountName -> Doc ann
lAccountNameDoc = accountNameDoc . locatedValue

accountNameDoc :: AccountName -> Doc ann
accountNameDoc = pretty . AccountName.toText

accountDoc :: DecimalLiteral -> Doc ann
accountDoc = decimalLiteralDoc . DecimalLiteral.setSignRequired

accountDocHelper :: Maybe (Max Int) -> Maybe (Max Word8) -> DecimalLiteral -> Doc ann
accountDocHelper mMaxDigitsBeforeDot mMaxAccountDecimals dl' =
  let dl = DecimalLiteral.setSignRequired dl'
      padFrontWithSpaces (Max maxChars) =
        let d = maxChars - charactersBeforeDot dl
         in (pretty (replicate d ' ') <>)
      padEndWithSpaces (Max maxDigits) =
        fill $
          -- +1 if there is a dot anywhere
          ( if maxDigits == 0
              then id
              else succ
          )
            (fromIntegral maxDigits + maybe 0 getMax mMaxDigitsBeforeDot)
   in maybe id padEndWithSpaces mMaxAccountDecimals $
        maybe id padFrontWithSpaces mMaxDigitsBeforeDot $
          decimalLiteralDoc dl

charactersBeforeDot :: DecimalLiteral -> Int
charactersBeforeDot (DecimalLiteral _ m e) =
  -- Assuming sign is required
  1 + max 1 (length (show m) - fromIntegral e)

decimalLiteralDoc :: DecimalLiteral -> Doc ann
decimalLiteralDoc = pretty . DecimalLiteral.format

lCurrencySymbolDoc :: GenLocated l CurrencySymbol -> Doc ann
lCurrencySymbolDoc = currencySymbolDoc . locatedValue

currencySymbolDoc :: CurrencySymbol -> Doc ann
currencySymbolDoc = pretty . currencySymbolText
