{ mkDerivation, autodocodec, autodocodec-yaml, base, centjes
, containers, envparse, lib, monad-logger, optparse-applicative
, path, path-io, really-safe-money, yaml
}:
mkDerivation {
  pname = "centjes-switzerland";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base centjes containers envparse
    monad-logger optparse-applicative path path-io really-safe-money
    yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-switzerland";
}
