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
import Centjes.Location
import Centjes.Module
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
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
      [ map importDoc moduleImports,
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
  | DecTransaction
  deriving (Show, Eq)

decType :: Declaration l -> DecType
decType = \case
  DeclarationComment {} -> DecComment
  DeclarationCurrency {} -> DecCurrency
  DeclarationAccount {} -> DecAccount
  DeclarationTransaction {} -> DecTransaction

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
currencySymbolDoc = pretty . currencySymbolText . locatedValue

quantisationFactorDoc :: GenLocated l DecimalLiteral -> Doc ann
quantisationFactorDoc = decimalLiteralDoc . DecimalLiteral.setSignOptional . locatedValue

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
accountNameDoc = pretty . accountNameText . locatedValue

accountDoc :: GenLocated l DecimalLiteral -> Doc ann
accountDoc = decimalLiteralDoc . DecimalLiteral.setSignRequired . locatedValue

decimalLiteralDoc :: DecimalLiteral -> Doc ann
decimalLiteralDoc = pretty . renderDecimalLiteral
