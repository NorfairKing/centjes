{ mkDerivation, alex, array, autodocodec, autodocodec-yaml, base
, deepseq, envparse, genvalidity, genvalidity-sydtest
, genvalidity-text, genvalidity-time, happy, lib, mtl
, optparse-applicative, path, path-io, prettyprinter, QuickCheck
, random, really-safe-money, really-safe-money-gen
, safe-coloured-text, sydtest, sydtest-discover, text, time
, validity, validity-text, validity-time, vector, yaml
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array autodocodec autodocodec-yaml base deepseq envparse mtl
    optparse-applicative path path-io prettyprinter random
    really-safe-money safe-coloured-text text time validity
    validity-text validity-time vector yaml
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest genvalidity-text
    genvalidity-time QuickCheck really-safe-money-gen sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
