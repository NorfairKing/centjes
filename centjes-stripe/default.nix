{ mkDerivation, autodocodec, base, bytestring, cassava, centjes
, centjes-gen, centjes-switzerland, conduit, containers, diagnose
, genvalidity, genvalidity-sydtest, genvalidity-text
, genvalidity-time, http-client, http-client-tls, http-types, lib
, monad-logger, opt-env-conf, opt-env-conf-test, path, path-io
, really-safe-money, scientific, sydtest, sydtest-discover, text
, time, transformers, unordered-containers, validity, validity-text
, validity-time, vector
}:
mkDerivation {
  pname = "centjes-stripe";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base bytestring cassava centjes centjes-switzerland
    conduit containers diagnose http-client http-client-tls http-types
    monad-logger opt-env-conf path path-io really-safe-money scientific
    text time transformers unordered-containers validity validity-text
    validity-time vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec base bytestring centjes centjes-gen conduit containers
    genvalidity genvalidity-sydtest genvalidity-text genvalidity-time
    monad-logger opt-env-conf-test path path-io really-safe-money
    sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-stripe";
}
