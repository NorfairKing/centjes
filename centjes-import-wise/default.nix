{ mkDerivation, base, bytestring, cassava, centjes, centjes-gen
, containers, diagnose, lib, monad-logger, opt-env-conf
, opt-env-conf-test, path, path-io, really-safe-money, sydtest
, sydtest-discover, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "centjes-import-wise";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava centjes containers diagnose monad-logger
    opt-env-conf path path-io really-safe-money text time
    unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring centjes centjes-gen containers opt-env-conf-test
    path path-io really-safe-money sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-wise";
}
