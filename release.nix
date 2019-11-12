{ gitrev ? "#devel"
, compiler ? "ghc881" # "default"
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = import (builtins.fetchGit nixpkgs) { };

  haskellPackages1 = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};
  haskellPackages = haskellPackages1.override {
    overrides = haskellPackagesNew: haskellpackagesOld: rec {
      hslogger = haskellPackagesNew.hslogger_1_3_1_0;
      RSA = haskellPackagesNew.RSA_2_4_1;
      text-format = haskellPackagesNew.callPackage ./text-format.nix { };
      authenticate-oauth = haskellPackagesNew.callPackage ./authenticate-oauth.nix { };
      deseo = haskellPackagesNew.callPackage ./deseo.nix { };
      vcr = vcrLib;
    };
  };

  vcrLib = haskellPackages.callPackage ./lib/default.nix { };
  app = haskellPackages.callPackage ./default.nix { };

  haskellDeps = ps: [
    ps.vcr
  ];

  ghcBase = haskellPackages.ghcWithPackages haskellDeps;

in
  app.overrideDerivation (oldAttrs: {
    src = builtins.filterSource
      (path: type: type != "directory" || baseNameOf path != ".git")
      ./.;
    preBuild = ''
      export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
      export GIT_REV=${gitrev}
      export GHC_BASE=${ghcBase}
      '';
  })

