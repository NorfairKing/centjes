{ mkDerivation, autodocodec, base, base64-bytestring, bytestring
, case-insensitive, cassava, centjes, centjes-gen, containers
, crypton, crypton-x509, crypton-x509-store, diagnose, http-client
, http-client-tls, http-types, lib, monad-logger, opt-env-conf
, opt-env-conf-test, path, path-io, really-safe-money, sydtest
, sydtest-discover, text, time, transformers, unordered-containers
, vector
}:
mkDerivation {
  pname = "centjes-import-wise";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec base base64-bytestring bytestring case-insensitive
    cassava centjes containers crypton crypton-x509 crypton-x509-store
    diagnose http-client http-client-tls http-types monad-logger
    opt-env-conf path path-io really-safe-money text time transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring centjes centjes-gen containers opt-env-conf-test
    path path-io really-safe-money sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-wise";
}
