{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [ base ];
  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
	haskellPackages.hpack
	pkgs.stack
    pkgs.ghcid
	haskellPackages.haskell-language-server
  ];
in
pkgs.stdenv.mkDerivation {
  name = "ptt-dev";
  nativeBuildInputs = nixPackages;
  # shellHook = ''
  # '';
}
