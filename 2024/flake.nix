{
  description = "A simple Haskell project";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              simple-haskell-project = hfinal.callCabal2nix "simple-haskell-project" ./. { };
            };
        };
        simple-haskell-project = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.simple-haskell-project;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.simple-haskell-project ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.simple-haskell-project;
        };
    in
    { inherit overlay; } // 
      inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
