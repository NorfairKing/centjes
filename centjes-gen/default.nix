{ mkDerivation, array, autodocodec, autodocodec-yaml, base
, bytestring, centjes, containers, deepseq, diagnose, envparse
, genvalidity, genvalidity-containers, genvalidity-path
, genvalidity-scientific, genvalidity-sydtest, genvalidity-text
, genvalidity-time, genvalidity-vector, lib, monad-logger, mtl
, optparse-applicative, path, path-io, prettyprinter
, prettyprinter-ansi-terminal, primitive, QuickCheck, random
, really-safe-money, really-safe-money-gen, safe-coloured-text
, safe-coloured-text-layout, safe-coloured-text-terminfo
, scientific, sydtest, sydtest-discover, text, time, unliftio
, validity, validity-containers, validity-path, validity-scientific
, validity-text, validity-time, validity-vector, vector, yaml
}:
mkDerivation {
  pname = "centjes-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array autodocodec autodocodec-yaml base bytestring centjes
    containers deepseq diagnose envparse genvalidity
    genvalidity-containers genvalidity-path genvalidity-scientific
    genvalidity-sydtest genvalidity-text genvalidity-time
    genvalidity-vector monad-logger mtl optparse-applicative path
    path-io prettyprinter prettyprinter-ansi-terminal primitive
    QuickCheck random really-safe-money really-safe-money-gen
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo scientific sydtest text time unliftio
    validity validity-containers validity-path validity-scientific
    validity-text validity-time validity-vector vector yaml
  ];
  testHaskellDepends = [
    base bytestring centjes containers genvalidity
    genvalidity-containers genvalidity-path genvalidity-scientific
    genvalidity-sydtest genvalidity-text genvalidity-time
    genvalidity-vector monad-logger path path-io QuickCheck
    really-safe-money really-safe-money-gen scientific sydtest text
    time vector
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
}
