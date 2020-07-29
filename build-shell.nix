{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc }:
with nixpkgs;

haskell.lib.buildStackProject {
    inherit ghc;
    name = "spotifyctl";
    buildInputs = [zlib.dev zlib.out glpk pcre haskell.compiler.ghc865];
    unpackPhase = "true";
} 
