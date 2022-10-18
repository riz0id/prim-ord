{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.prim-ord.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
    pkgs.clang
    pkgs.llvm
  ];
})
