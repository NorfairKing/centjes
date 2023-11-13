{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatTransaction,
    formatPosting,
    formatAccountName,
    formatAccount,
    formatAmount,
  )
where

import Centjes.Module
import Data.Text (Text)
import Money.Account as Money (Account)
import qualified Money.Account as Account
import Money.Amount as Money (Amount)
import qualified Money.Amount as Amount
import Prettyprinter
import Prettyprinter.Render.Text

formatModule :: Module -> Text
formatModule _ = ""

formatDeclaration :: Declaration -> Text
formatDeclaration _ = ""

formatTransaction :: Transaction -> Text
formatTransaction _ = ""

formatPosting :: Posting -> Text
formatPosting = renderDocText . postingDoc

formatAccountName :: AccountName -> Text
formatAccountName = renderDocText . accountNameDoc

formatAccount :: Money.Account -> Text
formatAccount = renderDocText . accountDoc

formatAmount :: Money.Amount -> Text
formatAmount = renderDocText . amountDoc

renderDocText :: Doc ann -> Text
renderDocText = renderStrict . layoutPretty layoutOptions
  where
    layoutOptions = LayoutOptions {layoutPageWidth = Unbounded}

postingDoc :: Posting -> Doc ann
postingDoc Posting {..} = accountNameDoc postingAccountName <+> accountDoc postingAmount

accountNameDoc :: AccountName -> Doc ann
accountNameDoc = pretty . unAccountName

accountDoc :: Money.Account -> Doc ann
accountDoc = pretty . Account.toMinimalQuantisations

amountDoc :: Money.Amount -> Doc ann
amountDoc = pretty . Amount.toMinimalQuantisations
