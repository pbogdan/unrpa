{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
with nixpkgs.pkgs.haskell.lib;
let hsPkgs =
      nixpkgs.pkgs.haskell.packages.${compiler}.override {
        overrides= self: super: {
          python-pickle = self.callPackage ./deps/python-pickle.nix { };
         };
      };
in justStaticExecutables (hsPkgs.callPackage ./unrpa.nix { })
