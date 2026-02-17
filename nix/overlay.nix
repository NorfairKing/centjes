final: prev:
with final.lib;
with final.haskell.lib;
{
  centjesRelease = final.symlinkJoin {
    name = "centjes";
    paths = attrValues final.centjesReleasePackages;
    passthru = {
      inherit (final) makeCheck;
      inherit (final) makeSwitzerlandVATPacket;
      inherit (final) makeSwitzerlandTaxesPacket;
      inherit (final.vimPlugins) centjes-vim;
    } // final.centjesReleasePackages;
  };

  makeCheck = src:
    final.stdenv.mkDerivation {
      name = "centjes-check";
      inherit src;
      buildCommand = ''
        set -e

        ${final.centjesReleasePackages.centjes}/bin/centjes --ledger $src/ledger.cent check > $out
      '';
    };

  makeSwitzerlandTaxesPacket =
    { name ? "taxes"
    , src
    , extraArgs ? [ ]
    }: final.stdenv.mkDerivation {
      inherit name;
      inherit src;
      buildInputs = [ final.centjesReleasePackages.centjes-switzerland ];
      buildCommand = ''
        mkdir -p $out

        centjes-switzerland \
          taxes \
          --config-file $src/switzerland.yaml \
          --base-dir $src \
          --zip-file $out/packet.zip \
          --packet-dir $out/packet \
          ${concatStringsSep " " extraArgs}
      '';
    };
  makeSwitzerlandVATPacket =
    { name ? "vat"
    , src
    , extraArgs ? [ ]
    }: final.stdenv.mkDerivation {
      inherit name;
      inherit src;
      buildInputs = [ final.centjesReleasePackages.centjes-switzerland ];
      buildCommand = ''
        mkdir -p $out

        centjes-switzerland \
          vat \
          --config-file $src/switzerland.yaml \
          --base-dir $src \
          --zip-file $out/packet.zip \
          --packet-dir $out/packet \
          ${concatStringsSep " " extraArgs}
      '';
    };

  centjesDependencyGraph = final.makeDependencyGraph {
    name = "centjes-dependency-graph";
    packages = builtins.attrNames final.centjesReleasePackages;
    format = "svg";
    inherit (final) haskellPackages;
  };

  centjesReleasePackages =
    builtins.mapAttrs
      (_: pkg: justStaticExecutables pkg)
      final.haskellPackages.centjesPackages;

  centjesNixosModuleDocs =
    let
      centjes-module = import ./nixos-module.nix
        {
          inherit (final.haskellPackages) opt-env-conf;
          inherit (final.centjesReleasePackages) centjes-docs-site;
        }
        {
          envname = "production";
        };
      eval = final.evalNixOSConfig {
        pkgs = final;
        modules = [
          centjes-module
          { system.stateVersion = "23.05"; }
        ];
      };
    in
    (final.nixosOptionsDoc {
      options = eval.options;
    }).optionsJSON;

  vimPlugins.centjes-vim = final.callPackage ../centjes-vim { };

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
          centjes-docs-site-pkg = overrideCabal (centjesPkg "centjes-docs-site") (old: {
            preConfigure = ''
              ${old.preConfigure or ""}

              export CENTJES_DOCS_DEPENDENCY_GRAPH="${final.centjesDependencyGraph}/centjes-dependency-graph.svg"
              export CENTJES_DOCS_NIXOS_MODULE_DOCS="${final.centjesNixosModuleDocs}/share/doc/nixos/options.json"
            '';
          });
          withLinksChecked = exeName: pkg:
            overrideCabal pkg (old: {
              postInstall = ''
                ${old.postInstall or ""}
        
                $out/bin/${exeName} &
                sleep 1                             
                ${final.linkcheck}/bin/linkcheck http://localhost:8080 --fetchers 2 --log-level Info --check-fragments
                ${final.seocheck}/bin/seocheck http://localhost:8080   --fetchers 2 --log-level LevelInfo
                ${final.killall}/bin/killall ${exeName}   
              '';
            });
          withStaticResources = pkg: resources: overrideCabal pkg (
            old:
            {
              preConfigure =
                let
                  copyResource = path: resource:
                    ''
                      local path="${path}"
                      mkdir --parents $(dirname "''$path")
                      ln -s ${resource} "''$path"             
                    '';
                  copyScript = concatStringsSep "\n" (mapAttrsToList copyResource resources);
                in
                ''
                  ${old.preConfigure or ""}                    
                  ${copyScript}
                '';
            }
          );

          centjes-docs-site = withLinksChecked "centjes-docs-site" (
            withStaticResources centjes-docs-site-pkg (
              {
                "static/bulma.css" = builtins.fetchurl {
                  url = "https://cdn.jsdelivr.net/npm/bulma@1.0.3/css/bulma.min.css";
                  sha256 = "sha256:0cswysl7zbphj1gxmxg0w5g4fns0fcslf1bzcbr3hcg4hb4hs1j7";
                };
                "static/favicon.ico" = builtins.fetchurl {
                  url = "https://cs-syd.eu/logo/res/favicon.ico";
                  sha256 = "sha256:0ahvcky6lrcpk2vd41558bjgh3x80mpkz4cl7smka534ypm5arz9";
                };
                "static/asciinema-player.js" = builtins.fetchurl {
                  url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.js";
                  sha256 = "sha256:092y2zl51z23jrl6mcqfxb64xaf9f2dx0j8kp69hp07m0935cz2p";
                };
                "static/asciinema-player.css" = builtins.fetchurl {
                  url = "https://github.com/asciinema/asciinema-player/releases/download/v2.6.1/asciinema-player.css";
                  sha256 = "sha256:1yi45fdps5mjqdwjhqwwzvlwxb4j7fb8451z7s6sdqmi7py8dksj";
                };
              }
            )
          );

          centjesPackages = {
            centjes = centjesPkg "centjes";
            centjes-gen = centjesPkg "centjes-gen";
            centjes-cryptocurrencies = centjesPkg "centjes-cryptocurrencies";
            centjes-stocks = centjesPkg "centjes-stocks";
            centjes-import-cornercard = centjesPkg "centjes-import-cornercard";
            centjes-import-neon = centjesPkg "centjes-import-neon";
            centjes-import-revolut = centjesPkg "centjes-import-revolut";
            centjes-switzerland = (centjesPkg "centjes-switzerland").overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
                final.makeWrapper
              ];
              installPhase = (old.installPhase or "") + ''
                wrapProgram $out/bin/centjes-switzerland --prefix PATH : ${final.lib.makeBinPath [ final.typst final.libxml2 ]}
              '';
            });
            inherit centjes-docs-site;
          };
          centjesRelease = final.symlinkJoin {
            name = "centjes-release";
            paths = attrValues self.centjesPackages;
          };
        in
        {
          diagnose = unmarkBroken (doJailbreak super.diagnose);
          zip = dontCheck (enableCabalFlag (super.zip.override { bzlib-conduit = null; }) "disable-bzip2");
          inherit centjesPackages;
          inherit centjesRelease;
        } // centjesPackages
    );
  });
}
