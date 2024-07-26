{ gitignore }:

final: prev:

let
  hsLib = final.haskell.lib.compose;

  ghcVersions = ["981"];

  defaultGHCVersion = "981";

  localHsPackages = {
    # Libraries
    prim-ord = ../../.;
  };

  mkLocalDerivation = hspkgs: name: path:
    let
      pkg = hspkgs.callCabal2nix name (gitignore.lib.gitignoreSource path) {};
    in
      haskell.lib.overrideCabal pkg (old: {
        doHaddock = true;
        doCheck = true;
      });

  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghc981 = prev.haskell.packages.ghc981.override {
        overrides = hfinal: hprev: (final.lib.mapAttrs (mkLocalDerivation hfinal) localHsPackages) // {
          prim-bool = hfinal.callPackage ../haskell-packages/prim-bool.nix {};
          prim-compat = hfinal.callPackage ../haskell-packages/prim-compat.nix {};
          prim-int = hfinal.callPackage ../haskell-packages/prim-int.nix {};
        };
      };
    };
  };

  mkDevShell = ghcVersion:
    let hsPkgs = haskell.packages."ghc${ghcVersion}";

        haskell-language-server = prev.haskell-language-server.override {
          supportedGhcVersions = [ ghcVersion ];
        };

        shell = hsPkgs.shellFor {
          name = "prim-ord-dev-ghc${ghcVersion}";

          doBenchmark = true;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            hsPkgs.ghc
            haskell-language-server
            final.hlint
            final.stack
            final.newman
          ] ++ final.lib.optionals (ghcVersion == defaultGHCVersion) [
            haskell.packages."ghc${defaultGHCVersion}".stylish-haskell
          ];

          src = null;
        };
    in { ${shell.name} = shell; };

  # Shell used to upload packages to hackage; contains a minimal set
  # of dependencies
  hackageUploadShell =
    let hsPkgs = haskell.packages."ghc${defaultGHCVersion}";

        shell = hsPkgs.shellFor {
          name = "prim-ord-hackage-upload-shell";
          doBenchmark = false;

          packages = pkgs: map (name: pkgs.${name}) (builtins.attrNames localHsPackages);

          buildInputs = [
            final.cabal-install
            final.cabal2nix
            final.curl
            final.findutils
            hsPkgs.ghc
          ];

          src = null;
        };
    in {
      ${shell.name} = shell;
    };
in {
  inherit ghcVersions defaultGHCVersion;

  inherit localHsPackages haskell mkDevShell hackageUploadShell;
}