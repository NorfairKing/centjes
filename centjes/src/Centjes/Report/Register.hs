{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Register
  ( Register (..),
    RegisterError (..),
    produceRegister,
  )
where

import Centjes.Convert
import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Control.DeepSeq
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import GHC.Generics (Generic)

newtype Register ann = Register {registerTransactions :: Vector (GenLocated ann (Transaction ann))}
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Register ann)

instance NFData ann => NFData (Register ann)

data RegisterError ann
  = RegisterReportConvertError !(ConvertError ann)
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance NFData ann => NFData (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterReportConvertError ce -> toReport ce

produceRegister :: Ledger ann -> Validation (RegisterError ann) (Register ann)
produceRegister = fmap Register . mapM registerTransaction . ledgerTransactions

registerTransaction ::
  GenLocated ann (Transaction ann) ->
  Validation (RegisterError ann) (GenLocated ann (Transaction ann))
registerTransaction = pure
