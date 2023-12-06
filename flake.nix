{
  description = "TXG: Chainweb transaction generator";
  # inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
  outputs = { self, flake-utils, hs-nix-infra }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      inherit (hs-nix-infra) nixpkgs haskellNix;
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          txgProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc963";
              projectFileName = "cabal.project";
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
      flake = pkgs.txgProject
      .flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in
     {
       packages = {
         default = flake.packages."txg:exe:txg";
         recursive = with hs-nix-infra.lib.recursive system;
           wrapRecursiveWithMeta "txg" "${wrapFlake self}.default";
       };

       devShell = flake.devShell;

       project = pkgs.txg;
    });
}

