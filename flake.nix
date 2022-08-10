{
  description = "A flake for building Liquid Haskell's REST-rewrite library";

  nixConfig = {
    extra-substituters = [
      "https://haskell-library-rest-rewrite.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-library-rest-rewrite.cachix.org-1:yZgBLpJclCtn2w2n7EHJ69EKhBJMix29XpG7K4p0Vvk="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    alejandra.url = "github:kamadorueda/alejandra";
    z3Source = {
      url = "github:Z3Prover/z3/z3-4.11.0";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    alejandra,
    z3Source,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      ghcName = "ghc8107";

      overlays = import ./nix/overlay-builders/overlays.nix {
        inherit z3Source ghcName;
        inherit (nixpkgs.legacyPackages.${system}.lib) composeManyExtensions;
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [overlays.default];
      };

      inherit (pkgs.haskell.packages.${ghcName}) shellFor ghcWithPackages rest-rewrite rest-rewrite-test;
    in {
      inherit overlays;
      formatter = alejandra.packages.${system}.default;

      apps = {
        update-flake = {
          type = "app";
          program = ./nix/scripts/update-flake.sh;
        };
        update-cabal = {
          type = "app";
          program = ./nix/scripts/update-cabal.sh;
        };
      };

      packages = {
        inherit rest-rewrite rest-rewrite-test;
        default = rest-rewrite;
      };

      devShells.default = shellFor {
        packages = ps: with ps; [rest-rewrite rest-rewrite-test];
        buildInputs = [(ghcWithPackages (ps: with ps; [apply-refact]))] ++( with pkgs; [cabal-install cabal2nix hlint]);
        # buildInputs = [(ghcWithPackages (ps: with ps; [cabal-install apply-refact]))] ++ (with pkgs; [hlint ormolu cabal2nix]);
      };
    });
}
