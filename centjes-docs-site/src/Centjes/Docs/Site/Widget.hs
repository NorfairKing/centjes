module Centjes.Docs.Site.Widget where

import Centjes.Docs.Site.Constants
import Data.Default
import Language.Haskell.TH.Syntax (Exp, Q)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFile :: String -> Q Exp
widgetFile =
  if development
    then widgetFileReload widgetFileSettings
    else widgetFileNoReload widgetFileSettings

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
