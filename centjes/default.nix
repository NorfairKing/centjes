{ mkDerivation, alex, array, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, containers, deepseq, envparse
, genvalidity, genvalidity-containers, genvalidity-path
, genvalidity-sydtest, genvalidity-text, genvalidity-time
, genvalidity-vector, happy, lib, monad-logger, mtl
, optparse-applicative, path, path-io, prettyprinter, QuickCheck
, random, really-safe-money, really-safe-money-gen
, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, scientific, sydtest
, sydtest-discover, text, time, validity, validity-containers
, validity-path, validity-scientific, validity-text, validity-time
, validity-vector, vector, yaml
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array autodocodec autodocodec-yaml base bytestring containers
    deepseq envparse monad-logger mtl optparse-applicative path path-io
    prettyprinter random really-safe-money safe-coloured-text
    safe-coloured-text-layout safe-coloured-text-terminfo scientific
    text time validity validity-containers validity-path
    validity-scientific validity-text validity-time validity-vector
    vector yaml
  ];
  libraryToolDepends = [ alex autoexporter happy ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers genvalidity genvalidity-containers genvalidity-path
    genvalidity-sydtest genvalidity-text genvalidity-time
    genvalidity-vector QuickCheck really-safe-money-gen sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
