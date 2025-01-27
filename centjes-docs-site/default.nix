{ mkDerivation, aeson, autoexporter, base, blaze-html, bytestring
, centjes, centjes-import-cornercard, centjes-import-neon
, centjes-import-revolut, centjes-switzerland, cmark, containers
, data-default, file-embed, filepath, fsnotify, lib, opt-env-conf
, opt-env-conf-test, path, path-io, prettyprinter
, safe-coloured-text, shakespeare, skylighting, sydtest
, sydtest-discover, template-haskell, template-haskell-reload, text
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
    aeson base blaze-html bytestring centjes centjes-import-cornercard
    centjes-import-neon centjes-import-revolut centjes-switzerland
    cmark containers data-default file-embed filepath fsnotify
    opt-env-conf path path-io prettyprinter safe-coloured-text
    shakespeare skylighting template-haskell template-haskell-reload
    text th-lift-instances wai-extra warp yesod yesod-autoreload
    yesod-sitemap yesod-static yesod-static-remote
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base opt-env-conf-test sydtest ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = lib.licenses.mit;
  mainProgram = "centjes-docs-site";
}
