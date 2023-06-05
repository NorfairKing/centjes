{ mkDerivation, base, brick, lib, mtl, random, sydtest, vector }:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base brick mtl random vector ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
