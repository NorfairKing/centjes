{ mkDerivation, base, bytestring, centjes, containers, genvalidity
, genvalidity-containers, genvalidity-path, genvalidity-scientific
, genvalidity-sydtest, genvalidity-text, genvalidity-time
, genvalidity-vector, lib, monad-logger, path, path-io, QuickCheck
, really-safe-money, really-safe-money-gen, scientific, sydtest
, sydtest-discover, text, time, vector
}:
mkDerivation {
  pname = "centjes-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base centjes genvalidity genvalidity-containers genvalidity-path
    genvalidity-scientific genvalidity-text genvalidity-time
    genvalidity-vector path QuickCheck really-safe-money-gen scientific
    sydtest text vector
  ];
  testHaskellDepends = [
    base bytestring centjes containers genvalidity-containers
    genvalidity-sydtest monad-logger path path-io QuickCheck
    really-safe-money really-safe-money-gen scientific sydtest text
    time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
}
