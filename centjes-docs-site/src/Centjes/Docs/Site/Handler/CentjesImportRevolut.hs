{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportRevolut
  ( getCentjesImportRevolutR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Revolut.OptParse as CLI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Options.Applicative.Help

getCentjesImportRevolutR :: Handler Html
getCentjesImportRevolutR = do
  DocPage {..} <- lookupPage "centjes-import-revolut"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc CLI.prefixedEnvironmentParser
      confHelpText = yamlDesc @CLI.Configuration
  defaultLayout $ do
    setCentjesTitle "centjes-import-revolut"
    setDescriptionIdemp "Documentation for the Centjes Importer for Revolut"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runFlagsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "centjes-import-revolut"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
