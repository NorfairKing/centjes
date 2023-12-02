{ mkDerivation, autodocodec, autodocodec-yaml, base, bytestring
, cassava, centjes, envparse, lib, optparse-applicative, path
, path-io, text, time, vector, yaml
}:
mkDerivation {
  pname = "centjes-import-revolut";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base bytestring cassava centjes
    envparse optparse-applicative path path-io text time vector yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-revolut";
}
