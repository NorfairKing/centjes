final: prev:
with final.lib;
with final.haskell.lib;
{

  twentyfortyeight = justStaticExecutables final.haskellPackages.twentyfortyeight;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        twentyfortyeight = generateOptparseApplicativeCompletion "twentyfortyeight" (
          buildFromSdist (overrideCabal
            (
              self.callPackage
                ../twentyfortyeight
                { }
            )
            (old: {
              doBenchmark = true;
              doHaddock = true;
              doCoverage = false;
              doHoogle = true;
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;

              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
                "--ghc-options=-optP-Wno-nonportable-include-path" # For macos
              ];
            })));
      }
    );
  });
}
