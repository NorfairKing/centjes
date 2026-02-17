{ mkDerivation, aeson, autodocodec, base, bytestring, centjes
, conduit, containers, http-client, http-client-tls, http-types
, lib, monad-logger, opt-env-conf, path, path-io, really-safe-money
, scientific, text, time, vector
}:
mkDerivation {
  pname = "centjes-stocks";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring centjes conduit containers
    http-client http-client-tls http-types monad-logger opt-env-conf
    path path-io really-safe-money scientific text time vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-stocks";
}
