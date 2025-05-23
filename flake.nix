{
  description = "A Template for Haskell Packages";
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";

    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    effectful-libs = {
      url = "github:tbidne/effectful-libs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
    };

    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                path = hlib.dontCheck prev.path_0_9_6;

                gitrev-typed = (
                  final.callHackageDirect {
                    pkg = "gitrev-typed";
                    ver = "0.1";
                    sha256 = "sha256-s7LEekR7NLe3CNhD/8uChnh50eGfaArrrtc5hoCtJ1A=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "relative-time"
                "fs-utils"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.effectful-libs}/lib" final [
                "concurrent-effectful"
                "effectful-utils"
                "fs-effectful"
                "optparse-effectful"
                "terminal-effectful"
              ];
          };
          compilerPkgs = {
            inherit compiler pkgs;
          };

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "cabal-monitor";
              root = ./.;

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  CABAL_MONITOR_HASH = "${self.rev or self.dirtyRev}";
                  CABAL_MONITOR_MODIFIED = "${builtins.toString self.lastModified}";
                  CABAL_MONITOR_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
                });

              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;
          };

          apps = {
            format = nix-hs-utils.format compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
