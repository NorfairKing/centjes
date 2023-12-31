{
  description = "centjes";
  nixConfig = {
    extra-substituters = "https://centjes.cachix.org";
    extra-trusted-public-keys = "centjes.cachix.org-1:H+Tih/7xFeUwTTWGFxgvaT8wUgracW4hPLzCAMgIiws=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    really-safe-money.url = "github:NorfairKing/really-safe-money";
    really-safe-money.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote";
    yesod-static-remote.flake = false;
    template-haskell-reload.url = "github:NorfairKing/template-haskell-reload";
    template-haskell-reload.flake = false;
    linkcheck.url = "github:NorfairKing/linkcheck";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck";
    seocheck.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , really-safe-money
    , yesod-autoreload
    , yesod-static-remote
    , template-haskell-reload
    , linkcheck
    , seocheck
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (import (validity + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (really-safe-money + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (template-haskell-reload + "/nix/overlay.nix"))
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (_:_: { makeDependencyGraph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph; })
          (_:_: { evalNixOSConfig = args: import (nixpkgs + "/nixos/lib/eval-config.nix") (args // { inherit system; }); })
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixOSModule = import ./nix/nixos-module.nix {
        inherit (pkgs.centjesReleasePackages) centjes-docs-site;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.centjes;
      checks.${system} = {
        package = self.packages.${system}.default;
        release = pkgs.haskellPackages.centjes;
        shell = self.devShells.${system}.default;
        e2e-test = import ./nix/e2e-test.nix {
          inherit (pkgs) nixosTest;
          centjes-nixos-module = self.nixosModules.${system}.default;
          inherit system;
        };
        pre-commit = pre-commit-hooks.lib.${ system}.run {
          src = ./.;
          hooks = {
            hlint. enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            typstfmt = {
              enable = true;
              files = "\\.typ$";
              entry = "${pkgs.typstfmt}/bin/typstfmt";
            };
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "centjes-shell";
        packages = p: builtins.attrValues p.centjesPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          pkg-config
          typst
          typstfmt
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;

        CENTJES_DOCS_DEPENDENCY_GRAPH = "${pkgs.centjesDependencyGraph}/centjes-dependency-graph.svg";
        CENTJES_DOCS_NIXOS_MODULE_DOCS = "${pkgs.centjesNixosModuleDocs}/share/doc/nixos/options.json";
      };
      nixosModules.${system} = {
        default = mkNixOSModule { envname = "production"; };
      };
      nixosModuleFactories.${system} = {
        default = mkNixOSModule;
      };
      nix-ci.cachix = {
        name = "centjes";
        public-key = "centjes.cachix.org-1:H+Tih/7xFeUwTTWGFxgvaT8wUgracW4hPLzCAMgIiws=";
      };
    };

}
