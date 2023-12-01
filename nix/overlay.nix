final: prev:
with final.lib;
with final.haskell.lib;
{

  centjes = justStaticExecutables final.haskellPackages.centjes;

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          centjesPkg = name: buildFromSdist
            (overrideCabal (self.callPackage ../${name} { })
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
              }));
          centjesPackages = {
            centjes = centjesPkg "centjes";
            centjes-gen = centjesPkg "centjes-gen";
            centjes-switzerland = centjesPkg "centjes-switzerland";
          };
          centjesRelease = final.symlinkJoin {
            name = "centjes-release";
            paths = attrValues self.centjesPackages;
          };
        in
        {
          diagnose = self.callCabal2nix "diagnose"
            (builtins.fetchGit {
              url = "https://github.com/Mesabloo/diagnose";
              rev = "0f17c071d0b00f56a5aabe06f756863d0baca13f";
            })
            { };
          inherit centjesPackages;
          inherit centjesRelease;
        } // centjesPackages
    );
  });
}
