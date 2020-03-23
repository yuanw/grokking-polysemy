{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
let
  bootstrap = import <nixpkgs> {};

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src {};
  myHaskellPackages = pkgs.haskellPackages;

  # myPackages = myHaskellPackages.callCabal2nix "project" ./blog.cabal {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
myHaskellPackages.shellFor {
  withHoogle = true;
  packages = p: [];
  inherit ((import ./pre-commit.nix).pre-commit-check) shellHook;
  buildInputs = with myHaskellPackages;
    [
      hlint
      ghcid
      cabal2nix
      ormolu
      cabal-install
      cabal-fmt
      (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
    ];
}