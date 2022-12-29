{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, unstable, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        ghcVersion = "925";

        # Project Definition
        overlays = [
          haskellNix.overlay
          (final: prev: {
            singletons-test = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc${ghcVersion}";
              modules = [{
                packages.singletons-test.components.exes.singletons-test = {
                  # Don't depend on GHC in build artifacts.  Otherwise GHC may
                  # be pulled in as a dependency, which causes docker images to
                  # balloon in size.
                  dontStrip = false;
                };
              }];
              shell = {
                tools = {
                  cabal = { index-state = "2022-11-16T00:00:00Z"; };
                  hlint = { index-state = "2022-11-16T00:00:00Z"; };
                  haskell-language-server = {
                    index-state = "2022-11-16T00:00:00Z";
                  };
                  # fourmolu = { index-state = "2022-11-16T00:00:00Z"; };
                };
                buildInputs = with final; [
                  zlib
                  bashInteractive
                  unstablePkgs.haskell.packages."ghc${ghcVersion}".fourmolu
                ];
              };
            };
          })
        ];

        # Pkgs
        unstablePkgs = import unstable { inherit system; };
        devPkgs = import nixpkgs { inherit system; };
        buildPkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        # Project flake and components
        flake = buildPkgs.singletons-test.flake { };
        executable = flake.packages."singletons-test:exe:singletons-test";
        test = flake.packages."singletons-test:test:test";

        # Development tools
        devTools = pkgs:
          with pkgs; [
            haskell.compiler."ghc${ghcVersion}"
            cabal-install
            (haskell.packages."ghc${ghcVersion}".haskell-language-server.override
              { })
            hlint
            haskellPackages.fourmolu
            zlib
            bashInteractive
          ];

        # Shells for development and CI
        devShell =
          buildPkgs.singletons-test.shellFor { buildInputs = devTools unstablePkgs; };
        devShellNoTools = buildPkgs.singletons-test.shellFor {
          withHoogle = false;
          buildInputs = [ unstablePkgs.cabal-install ];
        };
        devShellToolsOnly =
          unstablePkgs.mkShell { buildInputs = devTools unstablePkgs; };
        lintShell = unstablePkgs.mkShell {
          buildInputs = with unstablePkgs; [
            hlint
            haskell.packages."ghc${ghcVersion}".fourmolu
          ];
        };

      in flake // rec {
        defaultPackage = executable;
        packages = {
          inherit test devShell devShellNoTools devShellToolsOnly lintShell;
        };
      });
}
