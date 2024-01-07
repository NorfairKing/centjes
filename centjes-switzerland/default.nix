{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, centjes, centjes-gen, conduit
, containers, envparse, filepath, http-client, http-client-tls
, http-types, lib, monad-logger, mtl, optparse-applicative, path
, path-io, really-safe-money, sydtest, sydtest-aeson
, sydtest-discover, template-haskell, template-haskell-reload, text
, time, typed-process, vector, xml-conduit, yaml, zip
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    centjes conduit containers envparse http-client http-client-tls
    http-types monad-logger mtl optparse-applicative path path-io
    really-safe-money template-haskell template-haskell-reload text
    time typed-process vector xml-conduit yaml zip
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-yaml base centjes centjes-gen filepath monad-logger
    path path-io sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
