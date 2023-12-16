{-# LANGUAGE DerivingVia #-}

module Centjes.Formatting where

import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import qualified Data.Map as M
import qualified Data.Text as T
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
                f = fore $ if acc >= Account.zero then green else red
             in [ f $ accountChunk qf acc,
                  f $ currencySymbolChunk (currencySymbol c)
                ]
        )
        (M.toList accounts)

currencySymbolChunk :: CurrencySymbol -> Chunk
currencySymbolChunk = fore magenta . chunk . currencySymbolText

accountChunk :: QuantisationFactor -> Money.Account -> Chunk
accountChunk qf acc =
  fore (if acc >= Account.zero then green else red)
    . chunk
    . T.pack
    -- The maximum number of significant digits in an amount
    -- is the same as in the number 2^64, which is 20
    -- We add the possibility for a minus sign and a dot,
    -- that makes 22 digits.
    . printf "%22s"
    $ Account.format qf acc

timestampChunk :: Timestamp -> Chunk
timestampChunk = fore blue . chunk . Timestamp.toText

descriptionChunk :: Description -> Chunk
descriptionChunk (Description t) = fore yellow $ chunk t
