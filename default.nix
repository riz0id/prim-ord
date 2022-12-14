{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    haskell-language-server
    hlint
    prim-ord; 
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;
}

