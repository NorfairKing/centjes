{ mkDerivation, alex, array, autodocodec, autoexporter, base
, bytestring, containers, diagnose, dlist, fast-myers-diff
, fsnotify, graphviz, happy, lib, monad-logger, mtl, opt-env-conf
, path, path-io, pretty-show, prettyprinter
, prettyprinter-ansi-terminal, psqueues, really-safe-money
, safe-coloured-text, safe-coloured-text-layout
, safe-coloured-text-terminfo, text, time, unliftio, validity
, validity-containers, validity-path, validity-text, validity-time
, validity-vector, vector
}:
mkDerivation {
  pname = "centjes";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array autodocodec base bytestring containers diagnose dlist
    fast-myers-diff fsnotify graphviz monad-logger mtl opt-env-conf
    path path-io pretty-show prettyprinter prettyprinter-ansi-terminal
    psqueues really-safe-money safe-coloured-text
    safe-coloured-text-layout safe-coloured-text-terminfo text time
    unliftio validity validity-containers validity-path validity-text
    validity-time validity-vector vector
  ];
  libraryToolDepends = [ alex autoexporter happy ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes";
}
