{ mkDerivation, aeson, alex, array, autodocodec, autodocodec-yaml
, autoexporter, base, bytestring, containers, diagnose, envparse
, happy, lib, monad-logger, mtl, optparse-applicative, path
, path-io, prettyprinter, prettyprinter-ansi-terminal
, really-safe-money, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, text, time, unliftio, validity
, validity-containers, validity-path, validity-text, validity-time
, validity-vector, vector
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array autodocodec autodocodec-yaml base bytestring containers
    diagnose envparse monad-logger mtl optparse-applicative path
    path-io prettyprinter prettyprinter-ansi-terminal really-safe-money
    safe-coloured-text safe-coloured-text-layout
    safe-coloured-text-terminfo text time unliftio validity
    validity-containers validity-path validity-text validity-time
    validity-vector vector
  ];
  libraryToolDepends = [ alex autoexporter happy ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
