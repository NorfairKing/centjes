{ mkDerivation, base, brick, lib, mtl, random, vector }:
mkDerivation {
  pname = "twentyfortyeight";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base brick mtl random vector ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/twentyfortyeight#readme";
  license = "unknown";
  mainProgram = "twentyfortyeight";
}
