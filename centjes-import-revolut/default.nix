{ mkDerivation, base, bytestring, cassava, centjes, lib, text, time
, vector
}:
mkDerivation {
  pname = "centjes-import-revolut";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava centjes text time vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-revolut";
}
