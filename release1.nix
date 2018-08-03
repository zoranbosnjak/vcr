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

in
  { vcr = pkgs.haskellPackages.callPackage ./default.nix { };
  }

