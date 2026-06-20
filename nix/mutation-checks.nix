{ haskellPackages }:

# Mutation-testing checks for centjes.
#
# Each check instruments the listed `libraries` with the sydtest mutation
# plugin, builds the listed `tests` packages' suites against the instrumented
# libraries, runs them in mutation mode, and asserts every mutation is killed.
#
# OPT-IN, MODULE BY MODULE. Every library module currently carries a
#   {-# ANN module ("DisableMutations" :: String) #-}
# annotation, so the plugin instruments nothing and the check passes trivially
# with zero mutations. To start mutation-testing a module, delete its
# annotation: the check then enforces that the test suite KILLS every mutation
# the plugin generates for that module (and, via `assertNoneUncovered`, that
# every mutation is covered by a test). This grows coverage one module at a time
# without ever leaving the check red, and keeps each step's failures scoped to
# the module just enabled.
#
# Inspect a module's report once enabled with
#   nix build .#mutation-centjes.report.centjes   # report.txt / report.json
# the cheap coverage phase on its own with
#   nix build .#mutation-centjes.coverage.centjes-gen
# and run a diff-scoped mutation pass locally with
#   nix run .#mutation-centjes-diff
#
# NOTE: we only instrument centjes's own library code. Libraries that sydtest
# (and therefore the mutation plugin/driver) itself depends on cannot be
# instrumented — opt-env-conf and safe-coloured-text and its siblings — because
# the driver would load the mutated version of its own dependency. Keep those
# out of `libraries`.

let
  # Plugin-instrumentation tunables shared by every centjes mutation check.
  # Schema: Test.Syd.Mutation.Plugin.OptParse.MutationPluginConfig
  configFile = ./mutation.yaml;

  mutationCheck = args:
    haskellPackages.sydtest.mutationCheck ({ inherit configFile; } // args);
in
{
  # The core library. Its comprehensive property/golden/report suite lives in
  # centjes-gen; centjes-gen's library is generators (test support), so it is a
  # test package, not an instrumented one.
  #
  # We do NOT also list the `centjes` package's own test suite here: it is named
  # `centjes-test`, the same as centjes-gen's suite, and the mutation driver
  # flattens suite names into one namespace and rejects the duplicate
  # (DuplicateSuiteName). That suite only covers Centjes.OptParse, so the cost is
  # that mutations in Centjes.OptParse come back uncovered. To cover it too,
  # rename one of the two `centjes-test` suites so the names are unique.
  mutation-centjes = mutationCheck {
    name = "centjes";
    libraries = [ "centjes" ];
    tests = [ "centjes-gen" ];
    # assertAllKilled and assertNoneUncovered both default to true: as each
    # module is opted in (annotation removed), the check fails unless the suite
    # kills and covers every mutation generated for it.
    #
    # The mutationCheck default of 4 coverage/mutation children is a
    # conservative cap for suites with heavy per-test resources (e.g.
    # tmp-postgres). centjes's tests are pure and fast with no such resources,
    # so we run many more children in parallel to use the machine. mutationJobs
    # defaults to coverageJobs, so this raises both phases. Lower it if a
    # smaller/memory-constrained host OOMs (each child is capped at
    # testProcessMemLimit, 4g by default).
    coverageJobs = 64;
  };
}
