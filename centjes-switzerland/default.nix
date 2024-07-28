{ mkDerivation, aeson, aeson-pretty, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, centjes, centjes-gen, conduit
, containers, diagnose, filepath, genvalidity, genvalidity-sydtest
, genvalidity-time, http-client, http-client-tls, http-types, lib
, monad-logger, mtl, opt-env-conf, opt-env-conf-test, path, path-io
, pretty-show, QuickCheck, really-safe-money, really-safe-money-gen
, sydtest, sydtest-aeson, sydtest-discover, template-haskell
, template-haskell-reload, text, time, typed-process, validity
, validity-time, vector, xml-conduit, zip
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring centjes conduit
    containers diagnose http-client http-client-tls http-types
    monad-logger mtl opt-env-conf path path-io pretty-show
    really-safe-money template-haskell template-haskell-reload text
    time typed-process validity validity-time vector xml-conduit zip
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson autodocodec-yaml base centjes centjes-gen filepath
    genvalidity genvalidity-sydtest genvalidity-time monad-logger
    opt-env-conf opt-env-conf-test path path-io QuickCheck
    really-safe-money really-safe-money-gen sydtest sydtest-aeson text
    time xml-conduit
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
