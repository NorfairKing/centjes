{ mkDerivation, alex, array, base, deepseq, genvalidity
, genvalidity-sydtest, genvalidity-text, genvalidity-time, happy
, lib, mtl, prettyprinter, QuickCheck, random, really-safe-money
, really-safe-money-gen, sydtest, sydtest-discover, text, time
, validity, validity-text, validity-time, vector
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base deepseq mtl prettyprinter random really-safe-money text
    time validity validity-text validity-time vector
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
