{ mkDerivation, aeson, aeson-pretty, autodocodec, autoexporter
, base, bytestring, centjes, Chart, Chart-cairo, colour, conduit
, containers, diagnose, http-client, http-client-tls, http-types
, lib, monad-logger, mtl, opt-env-conf, path, path-io, pretty-show
, really-safe-money, text, time, typed-process, unliftio, validity
, validity-time, vector, xml-conduit, zip
}:
mkDerivation {
  pname = "centjes-chart";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty autodocodec base bytestring centjes Chart
    Chart-cairo colour conduit containers diagnose http-client
    http-client-tls http-types monad-logger mtl opt-env-conf path
    path-io pretty-show really-safe-money text time typed-process
    unliftio validity validity-time vector xml-conduit zip
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-chart";
}
