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
    formatRationalExpression,
    formatTransaction,
    moduleDoc,
    SyntaxElement (..),
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
import Money.Amount (Rounding (..))
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

formatRationalExpression :: RationalExpression l -> Text
formatRationalExpression = renderDocText . rationalExpressionDoc

formatTransaction :: Transaction l -> Text
formatTransaction = renderDocText . transactionDoc

renderDocText :: Doc SyntaxElement -> Text
renderDocText = renderStrict . layoutPretty layoutOptions
  where
    layoutOptions = LayoutOptions {layoutPageWidth = Unbounded}

moduleDoc :: Module l -> Doc SyntaxElement
moduleDoc Module {..} =
  mconcat $
    concat
      [ map lImportDoc moduleImports,
        [hardline | not (null moduleImports) && not (null moduleDeclarations)],
        go Nothing moduleDeclarations
      ]
  where
    go :: Maybe DecType -> [GenLocated l (Declaration l)] -> [Doc SyntaxElement]
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

lImportDoc :: GenLocated l (Import l) -> Doc SyntaxElement
lImportDoc = importDoc . locatedValue

importDoc :: Import l -> Doc SyntaxElement
importDoc (Import (Located _ fp)) =
  let pString = fromRelFile $ case splitExtension fp of
        Just (rest, ".cent") -> rest
        _ -> fp
   in annotate SyntaxKeyword "import" <+> annotate SyntaxImport (pretty pString) <> hardline

declarationDoc :: Declaration l -> Doc SyntaxElement
declarationDoc = \case
  DeclarationComment t -> commentDoc t
  DeclarationCurrency cd -> lCurrencyDeclarationDoc cd
  DeclarationAccount ad -> lAccountDeclarationDoc ad
  DeclarationTag ad -> lTagDeclarationDoc ad
  DeclarationPrice pd -> lPriceDeclarationDoc pd
  DeclarationTransaction t -> transactionDecDoc t

commentDoc :: GenLocated l Text -> Doc SyntaxElement
commentDoc (Located _ t) =
  annotate SyntaxComment $
    let ls = if T.null t then [""] else T.lines t
        commentLine l = "--" <+> pretty l
     in mconcat $ map commentLine ls

lCurrencyDeclarationDoc :: GenLocated l (CurrencyDeclaration l) -> Doc SyntaxElement
lCurrencyDeclarationDoc = currencyDeclarationDoc . locatedValue

currencyDeclarationDoc :: CurrencyDeclaration l -> Doc SyntaxElement
currencyDeclarationDoc CurrencyDeclaration {..} =
  annotate SyntaxKeyword "currency"
    <+> currencySymbolDoc (locatedValue currencyDeclarationSymbol)
    <+> quantisationFactorDoc currencyDeclarationQuantisationFactor

quantisationFactorDoc :: GenLocated l DecimalLiteral -> Doc SyntaxElement
quantisationFactorDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

lAccountDeclarationDoc :: GenLocated l (AccountDeclaration l) -> Doc SyntaxElement
lAccountDeclarationDoc = accountDeclarationDoc . locatedValue

accountDeclarationDoc :: AccountDeclaration l -> Doc SyntaxElement
accountDeclarationDoc AccountDeclaration {..} =
  mconcat $
    intersperse hardline $
      concat
        [ [ maybe
              id
              (\at -> (<+> lAccountTypeDoc at))
              accountDeclarationType
              ( annotate SyntaxKeyword "account"
                  <+> lAccountNameDoc accountDeclarationName
              )
          ],
          map (("  " <>) . accountExtraDoc . locatedValue) accountDeclarationExtras
        ]

accountExtraDoc :: AccountExtra l -> Doc SyntaxElement
accountExtraDoc =
  ("+" <+>) . \case
    AccountExtraAttachment a -> extraAttachmentDoc (locatedValue a)
    AccountExtraAssertion la -> lAccountAssertionDoc la
    AccountExtraTag lt -> lExtraTagDoc lt

lAccountAssertionDoc :: GenLocated l (AccountAssertion l) -> Doc SyntaxElement
lAccountAssertionDoc = accountAssertionDoc . locatedValue

accountAssertionDoc :: AccountAssertion l -> Doc SyntaxElement
accountAssertionDoc = \case
  AccountAssertionCurrency currencySymbol ->
    annotate SyntaxKeyword "assert"
      <+> annotate SyntaxKeyword "currency"
      <+> lCurrencySymbolDoc currencySymbol

lAccountTypeDoc :: GenLocated l AccountType -> Doc SyntaxElement
lAccountTypeDoc (Located _ at) = pretty $ AccountType.toText at

lTagDeclarationDoc :: GenLocated l (TagDeclaration l) -> Doc SyntaxElement
lTagDeclarationDoc = tagDeclarationDoc . locatedValue

tagDeclarationDoc :: TagDeclaration l -> Doc SyntaxElement
tagDeclarationDoc TagDeclaration {..} =
  annotate SyntaxKeyword "tag"
    <+> tagDoc (locatedValue tagDeclarationTag)

lPriceDeclarationDoc :: GenLocated l (PriceDeclaration l) -> Doc SyntaxElement
lPriceDeclarationDoc = priceDeclarationDoc . locatedValue

priceDeclarationDoc :: PriceDeclaration l -> Doc SyntaxElement
priceDeclarationDoc PriceDeclaration {..} =
  annotate SyntaxKeyword "price"
    <+> lTimestampDoc priceDeclarationTimestamp
    <+> lCurrencySymbolDoc priceDeclarationCurrencySymbol
    <+> lCostExpressionDoc priceDeclarationCost

lConversionRateDoc :: GenLocated l (RationalExpression l) -> Doc SyntaxElement
lConversionRateDoc = conversionRateDoc . locatedValue

conversionRateDoc :: RationalExpression l -> Doc SyntaxElement
conversionRateDoc = rationalExpressionDoc

transactionDecDoc :: GenLocated l (Transaction l) -> Doc SyntaxElement
transactionDecDoc = transactionDoc . locatedValue

transactionDoc :: Transaction l -> Doc SyntaxElement
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

lTimestampDoc :: GenLocated l Timestamp -> Doc SyntaxElement
lTimestampDoc = timestampDoc . locatedValue

timestampDoc :: Timestamp -> Doc SyntaxElement
timestampDoc = annotate SyntaxTimestamp . pretty . Timestamp.toString

descriptionDocs :: GenLocated l Description -> [Doc SyntaxElement]
descriptionDocs = map (annotate SyntaxDescription . pretty . ("| " <>)) . T.lines . unDescription . locatedValue

postingDocHelper :: Maybe (Max Int) -> Maybe (Max Int) -> Maybe (Max Word8) -> Posting l -> Doc SyntaxElement
postingDocHelper mMaxAccountNameWidth mMaxAccountWidth mMaxAccountDecimals Posting {..} =
  maybe id (\pe -> (<+> lRatioExpressionDoc pe)) postingRatio $
    maybe id (\ce -> (<+> ("@" <+> lCostExpressionDoc ce))) postingCost $
      (if postingReal then "*" else "!")
        <+> maybe id (fill . getMax) mMaxAccountNameWidth (lAccountNameDoc postingAccountName)
        <+> accountDocHelper mMaxAccountWidth mMaxAccountDecimals (locatedValue postingAccount)
        <+> lCurrencySymbolDoc postingCurrencySymbol

lCostExpressionDoc :: GenLocated l (CostExpression l) -> Doc SyntaxElement
lCostExpressionDoc (Located _ CostExpression {..}) =
  lConversionRateDoc costExpressionConversionRate
    <+> lCurrencySymbolDoc costExpressionCurrencySymbol

lRatioExpressionDoc :: GenLocated l (RatioExpression l) -> Doc SyntaxElement
lRatioExpressionDoc (Located _ RatioExpression {..}) =
  "~"
    <> ( case ratioExpressionInclusive of
           Nothing -> ""
           Just True -> "i"
           Just False -> "e"
       )
    <> ( case ratioExpressionRounding of
           Nothing -> ""
           Just RoundUp -> "u"
           Just RoundDown -> "d"
           Just RoundNearest -> "n"
       )
    <+> lRationalExpressionDoc ratioExpressionRationalExpression

lRationalExpressionDoc :: GenLocated l (RationalExpression l) -> Doc SyntaxElement
lRationalExpressionDoc = rationalExpressionDoc . locatedValue

rationalExpressionDoc :: RationalExpression l -> Doc SyntaxElement
rationalExpressionDoc RationalExpression {..} =
  mconcat
    [ lRationalDecimalLiteralDoc rationalExpressionNumerator,
      case rationalExpressionDenominator of
        Nothing -> mempty
        Just ld -> " /" <+> lRationalDecimalLiteralDoc ld,
      if rationalExpressionPercent then "%" else mempty
    ]

lRationalDecimalLiteralDoc :: GenLocated l DecimalLiteral -> Doc SyntaxElement
lRationalDecimalLiteralDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

transactionExtraDoc :: TransactionExtra l -> Doc SyntaxElement
transactionExtraDoc =
  ("+" <+>) . \case
    TransactionAttachment a -> extraAttachmentDoc (locatedValue a)
    TransactionAssertion a -> extraAssertionDoc (locatedValue a)
    TransactionTag t -> lExtraTagDoc t

extraAttachmentDoc :: ExtraAttachment l -> Doc SyntaxElement
extraAttachmentDoc (ExtraAttachment (Located _ (Attachment (Located _ fp)))) =
  annotate SyntaxKeyword "attach"
    <+> pretty (fromRelFile fp)

extraAssertionDoc :: ExtraAssertion l -> Doc SyntaxElement
extraAssertionDoc (ExtraAssertion (Located _ (AssertionEquals an (Located _ dl) cs))) =
  annotate SyntaxKeyword "assert"
    <+> lAccountNameDoc an
    <+> "="
    <+> accountDoc dl
    <+> lCurrencySymbolDoc cs

lExtraTagDoc :: GenLocated l (ExtraTag l) -> Doc SyntaxElement
lExtraTagDoc = extraTagDoc . locatedValue

extraTagDoc :: ExtraTag l -> Doc SyntaxElement
extraTagDoc (ExtraTag lt) =
  annotate SyntaxKeyword "tag"
    <+> tagDoc (locatedValue lt)

tagDoc :: Tag -> Doc SyntaxElement
tagDoc = pretty . Tag.toText

lAccountNameDoc :: GenLocated l AccountName -> Doc SyntaxElement
lAccountNameDoc = accountNameDoc . locatedValue

accountNameDoc :: AccountName -> Doc SyntaxElement
accountNameDoc = annotate SyntaxAccountName . pretty . AccountName.toText

accountDoc :: DecimalLiteral -> Doc SyntaxElement
accountDoc = decimalLiteralDoc . DecimalLiteral.setSignRequired

accountDocHelper :: Maybe (Max Int) -> Maybe (Max Word8) -> DecimalLiteral -> Doc SyntaxElement
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

decimalLiteralDoc :: DecimalLiteral -> Doc SyntaxElement
decimalLiteralDoc = annotate SyntaxDecimalLiteral . pretty . DecimalLiteral.toString

lCurrencySymbolDoc :: GenLocated l CurrencySymbol -> Doc SyntaxElement
lCurrencySymbolDoc = currencySymbolDoc . locatedValue

currencySymbolDoc :: CurrencySymbol -> Doc SyntaxElement
currencySymbolDoc = annotate SyntaxCurrencySymbol . pretty . currencySymbolText

data SyntaxElement
  = SyntaxImport
  | SyntaxKeyword
  | SyntaxComment
  | SyntaxDecimalLiteral
  | SyntaxTimestamp
  | SyntaxDescription
  | SyntaxCurrencySymbol
  | SyntaxAccountName
