{ mkDerivation, alex, array, base, happy, lib, mtl, random
, really-safe-money, sydtest, text, time, vector
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base mtl random really-safe-money text time vector
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base sydtest ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
