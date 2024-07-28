{ mkDerivation, base, bytestring, cassava, centjes, containers
, diagnose, lib, monad-logger, opt-env-conf, path, path-io
, really-safe-money, text, time, vector
}:
mkDerivation {
  pname = "centjes-import-neon";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava centjes containers diagnose monad-logger
    opt-env-conf path path-io really-safe-money text time vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-import-neon";
}
