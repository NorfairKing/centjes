{ mkDerivation, base, bytestring, centjes, containers, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-text, genvalidity-time, genvalidity-vector, lib
, monad-logger, path, path-io, QuickCheck, really-safe-money-gen
, sydtest, sydtest-discover, text, time, vector
}:
mkDerivation {
  pname = "centjes-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base centjes genvalidity genvalidity-containers genvalidity-path
    genvalidity-text genvalidity-time genvalidity-vector path
    QuickCheck really-safe-money-gen sydtest text vector
  ];
  testHaskellDepends = [
    base bytestring centjes containers genvalidity-containers
    genvalidity-sydtest monad-logger path path-io QuickCheck sydtest
    text time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
}
