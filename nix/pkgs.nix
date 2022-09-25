{ ghc ? "ghc922" }:

let
  nixpkgs = import ./nixpkgs.nix { };
in import nixpkgs {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import extensions/prim-bool.nix {
        inherit ghc;
      })
      (import extensions/prim-compat.nix {
        inherit ghc;
      })
      (import extensions/prim-ord.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}