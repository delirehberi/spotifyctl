{pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [pkgs.haskellPackages.ghcide];
}
