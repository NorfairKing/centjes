{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatTransaction,
  )
where

import Centjes.DecimalLiteral as DecimalLiteral
import Centjes.Location
import Centjes.Module
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
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
  vcat $
    concat
      [ map importDoc moduleImports,
        map declarationDoc moduleDeclarations
      ]

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
  DeclarationTransaction t -> transactionDecDoc t

commentDoc :: GenLocated l Text -> Doc ann
commentDoc (Located _ t) =
  let ls = if T.null t then [""] else T.lines t
      commentLine l = "--" <+> pretty l <> hardline
   in mconcat $ map commentLine ls

currencyDeclarationDoc :: GenLocated l (CurrencyDeclaration l) -> Doc ann
currencyDeclarationDoc (Located _ CurrencyDeclaration {..}) =
  "currency"
    <+> currencySymbolDoc currencyDeclarationSymbol
    <+> quantisationFactorDoc currencyDeclarationQuantisationFactor
      <> hardline

currencySymbolDoc :: GenLocated l CurrencySymbol -> Doc ann
currencySymbolDoc = pretty . unCurrencySymbol . locatedValue

quantisationFactorDoc :: GenLocated l DecimalLiteral -> Doc ann
quantisationFactorDoc = decimalLiteralDoc . (\dl -> dl {decimalLiteralSign = False}) . locatedValue

accountDeclarationDoc :: GenLocated l (AccountDeclaration l) -> Doc ann
accountDeclarationDoc (Located _ AccountDeclaration {..}) =
  "account"
    <+> accountNameDoc accountDeclarationName
      <> hardline

transactionDecDoc :: GenLocated l (Transaction l) -> Doc ann
transactionDecDoc = transactionDoc . locatedValue

transactionDoc :: Transaction l -> Doc ann
transactionDoc Transaction {..} =
  mconcat $
    map (<> hardline) $
      concat
        [ [timestampDoc transactionTimestamp],
          [ mconcat
              [ "  | ",
                descriptionDoc d
              ]
            | d <- maybeToList transactionDescription
          ],
          map (("  " <>) . postingDoc . locatedValue) transactionPostings
        ]

timestampDoc :: GenLocated l Timestamp -> Doc ann
timestampDoc = pretty . formatTime defaultTimeLocale "%F" . timestampDay . locatedValue

descriptionDoc :: GenLocated l Description -> Doc ann
descriptionDoc = pretty . unDescription . locatedValue

postingDoc :: Posting l -> Doc ann
postingDoc Posting {..} =
  fill
    60
    ( "*"
        <+> accountNameDoc postingAccountName
        <+> " "
    )
    <+> accountDoc postingAccount
    <+> currencySymbolDoc postingCurrencySymbol

accountNameDoc :: GenLocated l AccountName -> Doc ann
accountNameDoc = pretty . unAccountName . locatedValue

accountDoc :: GenLocated l DecimalLiteral -> Doc ann
accountDoc = decimalLiteralDoc . (\dl -> dl {decimalLiteralSign = True}) . locatedValue

decimalLiteralDoc :: DecimalLiteral -> Doc ann
decimalLiteralDoc = pretty . renderDecimalLiteral
