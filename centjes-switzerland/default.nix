{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, centjes, centjes-gen, containers, envparse, filepath
, lib, monad-logger, mtl, optparse-applicative, path, path-io
, really-safe-money, sydtest, sydtest-aeson, sydtest-discover, tar
, text, time, typed-process, vector, yaml
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring centjes
    containers envparse monad-logger mtl optparse-applicative path
    path-io really-safe-money tar text time typed-process vector yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-yaml base centjes centjes-gen filepath monad-logger
    path path-io sydtest sydtest-aeson
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
