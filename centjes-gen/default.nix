{ mkDerivation, base, bytestring, centjes, containers, diagnose
, genvalidity, genvalidity-containers, genvalidity-path
, genvalidity-sydtest, genvalidity-text, genvalidity-time
, genvalidity-vector, lib, monad-logger, mtl, opt-env-conf
, opt-env-conf-test, path, path-io, QuickCheck, really-safe-money
, really-safe-money-gen, safe-coloured-text, sydtest
, sydtest-discover, text, vector, yaml
}:
mkDerivation {
  pname = "centjes-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base centjes containers diagnose genvalidity genvalidity-containers
    genvalidity-path genvalidity-text genvalidity-time
    genvalidity-vector path QuickCheck really-safe-money
    really-safe-money-gen sydtest text vector
  ];
  testHaskellDepends = [
    base bytestring centjes containers genvalidity-containers
    genvalidity-sydtest genvalidity-text monad-logger mtl opt-env-conf
    opt-env-conf-test path path-io QuickCheck really-safe-money
    safe-coloured-text sydtest text yaml
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
}
