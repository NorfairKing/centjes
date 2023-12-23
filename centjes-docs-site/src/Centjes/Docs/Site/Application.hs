{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Centjes.Docs.Site.Application where

import Centjes.Docs.Site.Foundation
import Centjes.Docs.Site.Handler

mkYesodDispatch "App" resourcesApp
