{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportNeon
  ( getCentjesImportNeonR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Neon.OptParse as CLI
import qualified Env
import Options.Applicative
import Options.Applicative.Help

getCentjesImportNeonR :: Handler Html
getCentjesImportNeonR = do
  DocPage {..} <- lookupPage "centjes-import-neon"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc CLI.prefixedEnvironmentParser
      confHelpText = yamlDesc @CLI.Configuration
  defaultLayout $ do
    setCentjesTitle "centjes-import-neon"
    setDescriptionIdemp "Documentation for the Centjes Importer for Neon"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runFlagsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "centjes-import-neon"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
