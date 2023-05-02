{
  description = "Haskell 'pipes-attoparsec' library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { }))
            (hself: hsuper: { pipes-attoparsec = hself.callPackage ./. { }; });
        };
      };
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          pipes-attoparsec__ghc96 =
            pkgs.haskell.packages.ghc96.pipes-attoparsec;
          pipes-attoparsec__ghc94 =
            pkgs.haskell.packages.ghc94.pipes-attoparsec;
          pipes-attoparsec__ghc92 =
            pkgs.haskell.packages.ghc92.pipes-attoparsec;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.pipes-attoparsec__ghc96
              config.packages.pipes-attoparsec__ghc96.doc
              config.devShells.ghc96

              config.packages.pipes-attoparsec__ghc94
              config.packages.pipes-attoparsec__ghc94.doc
              config.devShells.ghc94

              config.packages.pipes-attoparsec__ghc92
              config.packages.pipes-attoparsec__ghc92.doc
              config.devShells.ghc92
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.pipes-attoparsec ];
              withHoogle = false;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc94;
          ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
          ghc94 = mkShellFor pkgs.haskell.packages.ghc94;
          ghc92 = mkShellFor pkgs.haskell.packages.ghc92;
        };
      };
    };
}
