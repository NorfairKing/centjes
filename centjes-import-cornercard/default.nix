{ mkDerivation, autodocodec, autodocodec-yaml, base, bytestring
, cassava, centjes, containers, diagnose, envparse, lib
, monad-logger, optparse-applicative, path, path-io
, really-safe-money, text, time, vector
}:
mkDerivation {
  pname = "centjes-import-cornercard";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring cassava centjes
    containers diagnose envparse monad-logger optparse-applicative path
    path-io really-safe-money text time vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-cornercard";
}
