{ gitrev ? "#devel"
, compiler ? "default"
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = import (builtins.fetchGit nixpkgs) {
    overlays = [ replaceGTK ];
  };

  replaceGTK = self: super: {
    wxGTK = super.wxGTK30;
  };

  haskellPackages1 = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages1.override {
    overrides = haskellPackagesNew: haskellpackagesOld: rec {
      wx = haskellPackagesNew.callPackage ./nix/wx.nix { };
      wxcore = haskellPackagesNew.callPackage ./nix/wxcore.nix { };
      wxdirect = haskellPackagesNew.callPackage ./nix/wxdirect.nix { };
      wxc = haskellPackagesNew.callPackage ./nix/wxc.nix { };
      modern-uri = haskellPackagesNew.callPackage ./nix/modern-uri.nix { };
      deseo = haskellPackagesNew.callPackage ./nix/deseo.nix { };
      vcr = vcrLib;
    };
  };

  vcrLib = haskellPackages.callPackage ./lib.nix { };
  vcrApp = haskellPackages.callPackage ./app.nix { };

  haskellDeps = ps: [
    ps.vcr
  ];

  ghcBase = haskellPackages.ghcWithPackages haskellDeps;

  drv = vcrApp.overrideDerivation (oldAttrs: {
    src = builtins.filterSource
      (path: type: type != "directory" || baseNameOf path != ".git")
      ./.;
    preBuild = ''
      export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
      export GIT_REV=${gitrev}
      export GHC_BASE=${ghcBase}
      export WXC_LIB=${haskellPackages.wxc}/lib
      '';
    postInstall = ''
      ghc -Wall -O2 test-tools/generator.hs
      ghc -Wall -O2 test-tools/receiver.hs
      cp test-tools/generator $out/bin/vcr-generator
      cp test-tools/receiver $out/bin/vcr-receiver
      mkdir $out/replay
      cp replay/* $out/replay
    '';
  });

  env = pkgs.stdenv.mkDerivation {
    name = "vcr-environment";
    buildInputs =
      vcrLib.env.nativeBuildInputs
      ++ [
        pkgs.haskellPackages.cabal-install
        pkgs.ghcid
        pkgs.cabal2nix
      ];
    shellHook = ''
      export SW_VERSION="devel"
      export GIT_REV=${gitrev}
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
      export WXC_LIB=${haskellPackages.wxc}/lib
    '';
  };

in
  if pkgs.lib.inNixShell then env else drv

