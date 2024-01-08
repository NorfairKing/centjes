{-# LANGUAGE DerivingVia #-}

module Centjes.Formatting where

import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import qualified Data.Map as M
import Data.Semigroup
import qualified Data.Text as T
import Data.Word
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Money.QuantisationFactor
import Text.Colour
import Text.Printf

accountNameChunk :: AccountName -> Chunk
accountNameChunk = fore white . chunk . accountNameText

multiAccountMaxWidth :: Money.MultiAccount currency -> Max Word8
multiAccountMaxWidth = foldMap accountWidth . MultiAccount.unMultiAccount

multiAccountChunks :: Money.MultiAccount (Currency ann) -> [[Chunk]]
multiAccountChunks = multiAccountChunksWithWidth Nothing

multiAccountChunksWithWidth :: Maybe (Max Word8) -> Money.MultiAccount (Currency ann) -> [[Chunk]]
multiAccountChunksWithWidth mWidth ma =
  let accounts = MultiAccount.unMultiAccount ma
   in map
        ( \(c, acc) ->
            let Located _ qf = currencyQuantisationFactor c
                f = fore $ if acc >= Account.zero then green else red
             in [ f $ accountChunkWithWidth mWidth qf acc,
                  f $ currencySymbolChunk (currencySymbol c)
                ]
        )
        (M.toList accounts)

currencySymbolChunk :: CurrencySymbol -> Chunk
currencySymbolChunk = fore magenta . chunk . currencySymbolText

accountWidth :: Money.Account -> Max Word8
accountWidth a =
  -- One extra for the dot and one for the minus sign
  Max . (+ 2) $
    ceiling (logBase 10 (fromIntegral (Amount.toMinimalQuantisations (Account.abs a))) :: Float)

accountChunk :: QuantisationFactor -> Money.Account -> Chunk
accountChunk = accountChunkWithWidth Nothing

accountChunkWithWidth :: Maybe (Max Word8) -> QuantisationFactor -> Money.Account -> Chunk
accountChunkWithWidth mWidth qf acc =
  fore (if acc >= Account.zero then green else red)
    . chunk
    . T.pack
    . maybe id (\(Max w) -> printf ("%" ++ show w ++ "s")) mWidth
    $ Account.format qf acc

timestampChunk :: Timestamp -> Chunk
timestampChunk = fore blue . chunk . Timestamp.toText

descriptionChunks :: Description -> [Chunk]
descriptionChunks (Description t) =
  map (fore yellow . chunk) (T.lines t)
