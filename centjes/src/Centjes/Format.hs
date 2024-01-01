{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatTransaction,
  )
where

import Centjes.AccountName
import Centjes.AccountType as AccountType
import Centjes.Location
import Centjes.Module
import qualified Centjes.Timestamp as Timestamp
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.DecimalLiteral as DecimalLiteral
import Path
import Prettyprinter
import Prettyprinter.Render.Text

formatModule :: Module l -> Text
formatModule = renderDocText . moduleDoc

formatDeclaration :: Declaration l -> Text
formatDeclaration = renderDocText . declarationDoc

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
    go :: Maybe DecType -> [Declaration l] -> [Doc ann]
    go _ [] = []
    go Nothing (d : ds) =
      -- No newline up front, but remember the declaration type
      declarationDoc d : go (Just (decType d)) ds
    go (Just dt) (d : ds) =
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
            -- Group price declarations together
            (DecPrice, DecPrice) -> False
            -- Don't group transactions together
            (DecTransaction, DecTransaction) -> True
            _ -> dt /= dt'
       in ( if insertEmptyLine
              then (hardline :)
              else id
          )
            $ declarationDoc d : go (Just dt') ds

data DecType
  = DecComment
  | DecCurrency
  | DecAccount
  | DecPrice
  | DecTransaction
  deriving (Show, Eq)

decType :: Declaration l -> DecType
decType = \case
  DeclarationComment {} -> DecComment
  DeclarationCurrency {} -> DecCurrency
  DeclarationAccount {} -> DecAccount
  DeclarationPrice {} -> DecPrice
  DeclarationTransaction {} -> DecTransaction

lImportDoc :: GenLocated l Import -> Doc ann
lImportDoc = importDoc . locatedValue

importDoc :: Import -> Doc ann
importDoc (Import fp) =
  let pString = fromRelFile $ case splitExtension fp of
        Just (rest, ".cent") -> rest
        _ -> fp
   in "import" <+> pretty pString <> hardline

declarationDoc :: Declaration l -> Doc ann
declarationDoc = \case
  DeclarationComment t -> commentDoc t
  DeclarationCurrency cd -> currencyDeclarationDoc cd
  DeclarationAccount ad -> accountDeclarationDoc ad
  DeclarationPrice pd -> priceDeclarationDoc pd
  DeclarationTransaction t -> transactionDecDoc t

commentDoc :: GenLocated l Text -> Doc ann
commentDoc (Located _ t) =
  let ls = if T.null t then [""] else T.lines t
      commentLine l = "--" <+> pretty l <> hardline
   in mconcat $ map commentLine ls

currencyDeclarationDoc :: GenLocated l (CurrencyDeclaration l) -> Doc ann
currencyDeclarationDoc (Located _ CurrencyDeclaration {..}) =
  "currency"
    <+> currencySymbolDoc (locatedValue currencyDeclarationSymbol)
    <+> quantisationFactorDoc currencyDeclarationQuantisationFactor
      <> hardline

quantisationFactorDoc :: GenLocated l DecimalLiteral -> Doc ann
quantisationFactorDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

accountDeclarationDoc :: GenLocated l (AccountDeclaration l) -> Doc ann
accountDeclarationDoc (Located _ AccountDeclaration {..}) =
  maybe
    id
    (\at -> (<+> lAccountTypeDoc at))
    accountDeclarationType
    ( "account"
        <+> lAccountNameDoc accountDeclarationName
    )
    <> hardline

lAccountTypeDoc :: GenLocated l AccountType -> Doc ann
lAccountTypeDoc (Located _ at) = pretty $ AccountType.toText at

priceDeclarationDoc :: GenLocated l (PriceDeclaration l) -> Doc ann
priceDeclarationDoc (Located _ PriceDeclaration {..}) =
  "price"
    <+> lTimestampDoc priceDeclarationTimestamp
    <+> lCurrencySymbolDoc priceDeclarationCurrencySymbol
    <+> lCostExpressionDoc priceDeclarationCost
      <> hardline

lConversionRateDoc :: GenLocated l DecimalLiteral -> Doc ann
lConversionRateDoc = conversionRateDoc . locatedValue

conversionRateDoc :: DecimalLiteral -> Doc ann
conversionRateDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional

transactionDecDoc :: GenLocated l (Transaction l) -> Doc ann
transactionDecDoc = transactionDoc . locatedValue

transactionDoc :: Transaction l -> Doc ann
transactionDoc Transaction {..} =
  mconcat $
    map (<> hardline) $
      concat
        [ [lTimestampDoc transactionTimestamp],
          map ("  " <>) $ maybe [] descriptionDocs transactionDescription,
          map (("  " <>) . postingDoc . locatedValue) transactionPostings,
          map (("  " <>) . transactionExtraDoc . locatedValue) transactionExtras
        ]

lTimestampDoc :: GenLocated l Timestamp -> Doc ann
lTimestampDoc = timestampDoc . locatedValue

timestampDoc :: Timestamp -> Doc ann
timestampDoc = pretty . Timestamp.toString

descriptionDocs :: GenLocated l Description -> [Doc ann]
descriptionDocs = map (pretty . ("| " <>)) . T.lines . unDescription . locatedValue

postingDoc :: Posting l -> Doc ann
postingDoc Posting {..} =
  maybe id (\pe -> (<+> ("~" <+> lPercentageExpressionDoc pe))) postingPercentage $
    maybe id (\ce -> (<+> ("@" <+> lCostExpressionDoc ce))) postingCost $
      (if postingReal then "*" else "!")
        <+> lAccountNameDoc postingAccountName
        <+> accountDoc (locatedValue postingAccount)
        <+> lCurrencySymbolDoc postingCurrencySymbol

lCostExpressionDoc :: GenLocated l (CostExpression l) -> Doc ann
lCostExpressionDoc (Located _ CostExpression {..}) =
  lConversionRateDoc costExpressionConversionRate
    <+> lCurrencySymbolDoc costExpressionCurrencySymbol

lPercentageExpressionDoc :: GenLocated l (PercentageExpression l) -> Doc ann
lPercentageExpressionDoc (Located _ PercentageExpression {..}) =
  lConversionRateDoc unPercentageExpression
    <> "%"

transactionExtraDoc :: TransactionExtra l -> Doc ann
transactionExtraDoc =
  ("+" <+>) . \case
    TransactionAttachment a -> attachmentDoc (locatedValue a)
    TransactionAssertion a -> assertionDoc (locatedValue a)

attachmentDoc :: Attachment l -> Doc ann
attachmentDoc (Attachment fp) = "attach" <+> pretty (fromRelFile (locatedValue fp))

assertionDoc :: Assertion l -> Doc ann
assertionDoc (AssertionEquals an (Located _ dl) cs) =
  "assert" <+> lAccountNameDoc an <+> "=" <+> accountDoc dl <+> lCurrencySymbolDoc cs

lAccountNameDoc :: GenLocated l AccountName -> Doc ann
lAccountNameDoc = accountNameDoc . locatedValue

accountNameDoc :: AccountName -> Doc ann
accountNameDoc = pretty . accountNameText

accountDoc :: DecimalLiteral -> Doc ann
accountDoc = decimalLiteralDoc . DecimalLiteral.setSignRequired

decimalLiteralDoc :: DecimalLiteral -> Doc ann
decimalLiteralDoc = pretty . DecimalLiteral.format

lCurrencySymbolDoc :: GenLocated l CurrencySymbol -> Doc ann
lCurrencySymbolDoc = currencySymbolDoc . locatedValue

currencySymbolDoc :: CurrencySymbol -> Doc ann
currencySymbolDoc = pretty . currencySymbolText
