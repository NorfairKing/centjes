{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesSwitzerland
  ( getCentjesSwitzerlandR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Switzerland.OptParse as CLI
import qualified Env
import Options.Applicative
import Options.Applicative.Help

getCentjesSwitzerlandR :: Handler Html
getCentjesSwitzerlandR = do
  DocPage {..} <- lookupPage "centjes-switzerland"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc CLI.prefixedEnvironmentParser
      confHelpText = yamlDesc @CLI.Configuration
  defaultLayout $ do
    setCentjesTitle "centjes-switzerland"
    setDescriptionIdemp "Documentation for the Centjes Reporter for Revolut"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runFlagsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "centjes-switzerland"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
