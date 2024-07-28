{ mkDerivation, aeson, autodocodec, autodocodec-yaml, autoexporter
, base, bytestring, centjes, centjes-import-cornercard
, centjes-import-neon, centjes-import-revolut, centjes-switzerland
, cmark-gfm, containers, data-default, file-embed, filepath
, fsnotify, lib, opt-env-conf, opt-env-conf-test, path, path-io
, safe-coloured-text, shakespeare, sydtest, sydtest-discover
, template-haskell, template-haskell-reload, text
, th-lift-instances, wai-extra, warp, yesod, yesod-autoreload
, yesod-sitemap, yesod-static, yesod-static-remote
}:
mkDerivation {
  pname = "centjes-docs-site";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring centjes
    centjes-import-cornercard centjes-import-neon
    centjes-import-revolut centjes-switzerland cmark-gfm containers
    data-default file-embed filepath fsnotify opt-env-conf path path-io
    safe-coloured-text shakespeare template-haskell
    template-haskell-reload text th-lift-instances wai-extra warp yesod
    yesod-autoreload yesod-sitemap yesod-static yesod-static-remote
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = lib.licenses.mit;
  mainProgram = "centjes-docs-site";
}
