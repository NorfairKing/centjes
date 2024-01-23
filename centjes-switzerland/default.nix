{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, centjes, centjes-gen, conduit
, containers, diagnose, envparse, filepath, genvalidity
, genvalidity-sydtest, genvalidity-time, http-client
, http-client-tls, http-types, lib, monad-logger, mtl
, optparse-applicative, path, path-io, QuickCheck
, really-safe-money, really-safe-money-gen, sydtest, sydtest-aeson
, sydtest-discover, template-haskell, template-haskell-reload, text
, time, typed-process, validity, validity-time, vector, xml-conduit
, yaml, zip
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec autodocodec-yaml base bytestring
    centjes conduit containers diagnose envparse http-client
    http-client-tls http-types monad-logger mtl optparse-applicative
    path path-io really-safe-money template-haskell
    template-haskell-reload text time typed-process validity
    validity-time vector xml-conduit yaml zip
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson autodocodec-yaml base centjes centjes-gen filepath
    genvalidity genvalidity-sydtest genvalidity-time monad-logger path
    path-io QuickCheck really-safe-money really-safe-money-gen sydtest
    sydtest-aeson time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
