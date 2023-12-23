{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.Centjes
  ( getCentjesR,
  )
where

import Autodocodec
import Centjes.Docs.Site.Handler.Import hiding (Header)
import Centjes.OptParse as CLI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Yaml.Builder as Yaml
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

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = CLI.runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "centjes"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
