{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.Centjes
  ( getCentjesR,
    getCentjesCommandR,
  )
where

import Centjes.Docs.Site.Handler.Import hiding (Header)
import Centjes.OptParse as CLI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Options.Applicative.Help

getCentjesR :: Handler Html
getCentjesR = do
  DocPage {..} <- lookupPage "centjes"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc CLI.prefixedEnvironmentParser
      confHelpText = yamlDesc @CLI.Configuration
  defaultLayout $ do
    setCentjesTitle "centjes"
    setDescriptionIdemp "Documentation for the Centjes tool"
    $(widgetFile "args")

getCentjesCommandR :: Text -> Handler Html
getCentjesCommandR cmd = do
  DocPage {..} <- lookupPage' ["centjes", cmd]
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = "This command does not use any extra environment variables." :: String
      confHelpText = "This command admits no extra configuration." :: String
  defaultLayout $ do
    setCentjesTitle $ toHtml docPageTitle
    setDescriptionIdemp $ "Documentation for the " <> cmd <> " subcommand of the centjes tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = CLI.runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "centjes"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
