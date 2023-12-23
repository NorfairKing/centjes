{-# LANGUAGE TemplateHaskell #-}

module Centjes.Docs.Site.Assets where

import Centjes.Docs.Site.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "assets"
  ( let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
     in [ -- embedDir "content/assets",
          remoteStatic "asciinema-player.js" "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js",
          remoteStatic "asciinema-player.css" "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css",
          remoteStatic "bulma.css" "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css",
          remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico"
        ]
  )
