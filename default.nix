{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
let hsPkgs =
      nixpkgs.pkgs.haskell.packages.${compiler}.override {
        overrides= self: super: {
          python-pickle = self.callPackage ./deps/python-pickle.nix { };
         };
      };
in hsPkgs.callPackage ./unrpa.nix { }
