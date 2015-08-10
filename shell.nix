let
  pkgs = import <nixpkgs> {};
  hspkgs = pkgs.haskell-ng.packages.ghc7101.override {
     overrides = self: super: {
       minipaste = self.callPackage ./. {};
      };
   };
in
  hspkgs.minipaste.env
