{ mkDerivation, base, bytestring, centjes, lib, text }:
mkDerivation {
  pname = "centjes-import-revolut";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring centjes text ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-revolut";
}
