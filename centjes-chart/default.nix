{ mkDerivation, autoexporter, base, centjes, centjes-gen, Chart
, Chart-diagrams, colour, containers, diagnose, genvalidity
, genvalidity-sydtest, genvalidity-time, genvalidity-vector, lib
, monad-logger, opt-env-conf, path, path-io, really-safe-money
, really-safe-money-gen, sydtest, sydtest-discover, text, time
, validity, validity-time, validity-vector, vector
}:
mkDerivation {
  pname = "centjes-chart";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base centjes Chart Chart-diagrams colour containers diagnose
    monad-logger opt-env-conf path path-io really-safe-money text time
    validity validity-time validity-vector vector
  ];
  libraryToolDepends = [ autoexporter ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base centjes centjes-gen containers genvalidity genvalidity-sydtest
    genvalidity-time genvalidity-vector monad-logger path path-io
    really-safe-money really-safe-money-gen sydtest time vector
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/centjes#readme";
  license = "unknown";
  mainProgram = "centjes-chart";
}
