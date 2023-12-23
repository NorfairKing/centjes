{ mkDerivation, autodocodec, autodocodec-yaml, autoexporter, base
, bytestring, centjes, centjes-import-revolut, cmark-gfm
, containers, data-default, envparse, file-embed, filepath
, fsnotify, lib, optparse-applicative, path, path-io, shakespeare
, template-haskell, template-haskell-reload, text
, th-lift-instances, time, wai-extra, warp, yesod, yesod-autoreload
, yesod-sitemap, yesod-static, yesod-static-remote
}:
mkDerivation {
  pname = "centjes-docs-site";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring centjes
    centjes-import-revolut cmark-gfm containers data-default envparse
    file-embed filepath fsnotify optparse-applicative path path-io
    shakespeare template-haskell template-haskell-reload text
    th-lift-instances time wai-extra warp yesod yesod-autoreload
    yesod-sitemap yesod-static yesod-static-remote
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = lib.licenses.mit;
  mainProgram = "centjes-docs-site";
}
