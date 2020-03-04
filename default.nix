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
      wx = haskellPackagesNew.callPackage ./wx.nix { };
      wxcore = haskellPackagesNew.callPackage ./wxcore.nix { };
      wxdirect = haskellPackagesNew.callPackage ./wxdirect.nix { };
      wxc = haskellPackagesNew.callPackage ./wxc.nix { };
      req = haskellPackagesNew.callPackage ./req.nix { };
      modern-uri = haskellPackagesNew.callPackage ./modern-uri.nix { };

      deseo = haskellPackagesNew.callPackage ./deseo.nix { };
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
      '';
    postInstall = ''
      ghc -Wall -O2 generator/generator.hs
      cp generator/generator $out/bin
    '';
  });

  env = pkgs.stdenv.mkDerivation {
    name = "vcr-environment";
    buildInputs =
      vcrLib.env.nativeBuildInputs
      ++ [
        pkgs.haskellPackages.cabal-install
        pkgs.ghcid
      ];
    shellHook = ''
      export SW_VERSION="devel"
      export GIT_REV=${gitrev}
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
    '';
  };

in
  if pkgs.lib.inNixShell then env else drv

