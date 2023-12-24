{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, centjes, containers, envparse, lib, monad-logger
, optparse-applicative, path, path-io, really-safe-money, text
, time, typed-process, vector, yaml
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring centjes
    containers envparse monad-logger optparse-applicative path path-io
    really-safe-money text time typed-process vector yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
