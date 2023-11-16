{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatImport,
    formatTransaction,
    formatPosting,
    formatAccountName,
    formatAccount,
  )
where

import Centjes.Module
import Data.String
import Data.Text (Text)
import Path
import Prettyprinter
import Prettyprinter.Render.Text

formatModule :: Module -> Text
formatModule = renderDocText . moduleDoc

formatDeclaration :: Declaration -> Text
formatDeclaration = renderDocText . declarationDoc

formatImport :: Import -> Text
formatImport = renderDocText . importDoc

formatTransaction :: Transaction -> Text
formatTransaction = renderDocText . transactionDoc

formatPosting :: Posting -> Text
formatPosting = renderDocText . postingDoc

formatAccountName :: AccountName -> Text
formatAccountName = renderDocText . accountNameDoc

formatAccount :: Rational -> Text
formatAccount = renderDocText . accountDoc

renderDocText :: Doc ann -> Text
renderDocText = renderStrict . layoutPretty layoutOptions
  where
    layoutOptions = LayoutOptions {layoutPageWidth = Unbounded}

moduleDoc :: Module -> Doc ann
moduleDoc Module {..} =
  vcat $
    concat
      [ map importDoc moduleImports,
        map declarationDoc moduleDeclarations
      ]

importDoc :: Import -> Doc ann
importDoc (Import fp) = "import" <+> fromString (fromRelFile fp) <> "\n"

declarationDoc :: Declaration -> Doc ann
declarationDoc = \case
  DeclarationCurrency cd -> currencyDeclarationDoc cd
  DeclarationTransaction t -> transactionDoc t

currencyDeclarationDoc :: CurrencyDeclaration -> Doc ann
currencyDeclarationDoc CurrencyDeclaration {..} =
  "currency"
    <+> currencySymbolDoc currencyDeclarationSymbol
    <+> pretty (show currencyDeclarationFactor) <> "\n"

currencySymbolDoc :: CurrencySymbol -> Doc ann
currencySymbolDoc = pretty . unCurrencySymbol

transactionDoc :: Transaction -> Doc ann
transactionDoc Transaction {..} =
  mconcat $
    concat
      [ [timestampDoc transactionTimestamp <> "\n"],
        [ mconcat
            [ "  | ",
              descriptionDoc transactionDescription,
              "\n"
            ]
          | not (nullDescription transactionDescription)
        ],
        map (("  " <>) . postingDoc) transactionPostings
      ]

timestampDoc :: Timestamp -> Doc ann
timestampDoc = pretty . show

descriptionDoc :: Description -> Doc ann
descriptionDoc = pretty . unDescription

postingDoc :: Posting -> Doc ann
postingDoc Posting {..} =
  "*"
    <+> accountNameDoc postingAccountName
    <+> " "
    <+> accountDoc postingAccount
    <+> currencySymbolDoc postingCurrencySymbol <> "\n"

accountNameDoc :: AccountName -> Doc ann
accountNameDoc = pretty . unAccountName

accountDoc :: Rational -> Doc ann
accountDoc = pretty . show -- TODO
