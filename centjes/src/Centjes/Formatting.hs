{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Formatting where

import Centjes.Ledger
import Centjes.Location
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Money.QuantisationFactor
import Text.Colour
import Text.Printf

accountNameChunk :: AccountName -> Chunk
accountNameChunk = fore white . chunk . accountNameText

multiAccountChunks :: Money.MultiAccount (Currency ann) -> [[Chunk]]
multiAccountChunks ma =
  let accounts = MultiAccount.unMultiAccount ma
   in map
        ( \(c, acc) ->
            let Located _ qf = currencyQuantisationFactor c
             in [ accountChunk qf acc,
                  currencySymbolChunk (currencySymbol c)
                ]
        )
        (M.toList accounts)

currencySymbolChunk :: CurrencySymbol -> Chunk
currencySymbolChunk = fore magenta . chunk . currencySymbolText

accountChunk :: QuantisationFactor -> Money.Account -> Chunk
accountChunk qf a =
  fore (if a >= Account.zero then green else red)
    . chunk
    . T.pack
    . printf "%10s"
    $ Account.format qf a

timestampChunk :: Timestamp -> Chunk
timestampChunk (Timestamp d) = fore blue $ chunk $ T.pack $ formatTime defaultTimeLocale "%F" d

descriptionChunk :: Description -> Chunk
descriptionChunk (Description t) = fore yellow $ chunk t
