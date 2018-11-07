{ withHoogle ? false }:

let

  config = {
      packageOverrides = pkgs: rec {
          haskellPackages = pkgs.haskellPackages.override {
              overrides = haskellPackagesNew: haskellPackagesOld: rec {
                  hslogger = haskellPackagesNew.callPackage ./hslogger-1.2.11.nix { };
              };
          };
      };
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { inherit config; };

  haskellPackages = if withHoogle
    then ( pkgs.haskellPackages.override {
        overrides = (self: super:
            {
                ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                ghcWithPackages = self.ghc.withPackages;
            }
         ); } )
    else pkgs.haskellPackages;

  drv = haskellPackages.callPackage ./generated.nix { };

in

  if pkgs.lib.inNixShell then drv.env else drv

