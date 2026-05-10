{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    niceHaskell = {
      url = "github:saygo-png/nice-nixpkgs-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    systems = {
      url = "path:./systems.nix";
      flake = false;
    };
  };
  outputs = {
    nixpkgs,
    systems,
    niceHaskell,
    hs-bindgen,
    ...
  }: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system:
      import nixpkgs {
        inherit system;
        overlays = [hs-bindgen.overlays.default];
      });
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});

    program = system: pkgs:
      pkgs.callPackage ./package.nix {
        niceHaskell = niceHaskell.outputs.niceHaskell.${system};
      };
  in {
    packages = eachSystem (system: pkgs: {
      "saybar" = program system pkgs;
      default = program system pkgs;
    });
  };
}
/*
{
  description = "FokShell - a Haskell-configurable shell.";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          # This overlay adds our project to pkgs
          fokshell =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc96";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.fokshell.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // rec {
      # Built by `nix build .`
      packages.hetmanshell = flake.packages."fokshell:exe:hetmanshell";
      packages.default = packages.hetmanshell;
      packages.fokshell = pkgs.fokshell.hsPkgs.fokshell.components.library;
    });
}*/
