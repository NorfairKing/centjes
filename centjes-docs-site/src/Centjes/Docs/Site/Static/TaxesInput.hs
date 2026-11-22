{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Docs.Site.Static.TaxesInput (docsTaxesInput) where

import Centjes.Switzerland.Report.Taxes.Types (TaxesInput)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (lift, runIO)
import OptEnvConf (Parser, settingsParser)
import OptEnvConf.Args (emptyArgs)
import OptEnvConf.Capability (allCapabilities)
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Run (runParserOn)

-- [check:ref DocsSetAsideInput]
--
-- The taxes settings of the set-aside example on the docs page.
-- They come from running the settings parser on the example configuration,
-- so that every other setting is at the default that a user would get.
-- The rendering of a docs page is pure, because it also runs at runtime in
-- development, so the parser, which runs in IO, runs at compile time.
docsTaxesInput :: TaxesInput
docsTaxesInput =
  $( do
       let config :: JSON.Object
           config =
             KeyMap.fromList
               [ ("first-name", JSON.String "John"),
                 ("last-name", JSON.String "Doe"),
                 ("ahv-id", JSON.String "7561111222233"),
                 ("year", JSON.Number 2026),
                 ("third-pillar-assets-account", JSON.String "assets:pillar-3a")
               ]
       errOrTaxesInput <-
         runIO $
           runParserOn
             allCapabilities
             Nothing
             (settingsParser :: Parser TaxesInput)
             emptyArgs
             EnvMap.empty
             (Just config)
       case errOrTaxesInput of
         Left errs -> fail $ unlines ["The docs taxes settings do not parse:", show errs]
         Right taxesInput -> lift taxesInput
   )
