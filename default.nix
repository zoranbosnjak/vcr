{ sources ? import ./nix/sources.nix
, inShell ? null
, withHoogle ? false
, strip ? true
, static ? false    # build static binary
}:

let
  packages = import sources.nixpkgs {
    overlays = [ replaceGTK ];
  };
  pkgs = if static == true
    then packages.pkgsMusl.pkgsMusl
    else packages;

  replaceGTK = self: super: {
    wxGTK = super.wxGTK32;
  };

  deps = with pkgs; [
    which
    # ...
  ];

  haskellPackages = with pkgs.haskell.lib; pkgs.haskellPackages.override {
    overrides = self: super:
      let
        fixGHC = pkg:
          if static == true
          then
            pkg.override {
              enableRelocatedStaticLibs = true;
              enableShared = false;
              enableDwarf = false;
            }
          else
            pkg;
      in {
        ghc = fixGHC super.ghc;
        buildHaskellPackages = super.buildHaskellPackages.override (oldBuildHaskellPackages: {
          ghc = fixGHC oldBuildHaskellPackages.ghc;
        });
        # haskellPackage1 = self.callPackage ./nix/myPackage1.nix { };
        # ...

        ekg = self.callPackage ./nix/extra/ekg.nix { };
        ekg-json = self.callPackage ./nix/extra/ekg-json.nix { };

        #wx = self.callPackage ./nix/extra/wx.nix { };
        #wxcore = self.callPackage ./nix/extra/wxcore.nix { };
        #wxdirect = self.callPackage ./nix/extra/wxdirect.nix { };
        #wxc = self.callPackage ./nix/extra/wxc.nix { };

        #keera-hails-reactivevalues = haskellPackagesNew.callPackage ./nix/keera-hails-reactivevalues.nix { };
        #keera-hails-reactive-cbmvar = haskellPackagesNew.callPackage ./nix/keera-hails-reactive-cbmvar.nix { };
        #keera-hails-reactive-wx = haskellPackagesNew.callPackage ./nix/keera-hails-reactive-wx.nix { };
        #keera-hails-mvc-view = haskellPackagesNew.callPackage ./nix/keera-hails-mvc-view.nix { };

        deseo = self.callPackage ./nix/extra/deseo.nix { };
  };};

  buildExports = ''
    export LC_ALL=C.UTF-8
    export GHC_BASE=$(which ghc | cut -d '/' -f-4)
  '';

  drv1 = haskellPackages.callCabal2nix "vcr" ./. { };

  drv2 = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type:
          (type != "directory" || baseNameOf path != ".git")
          && (type != "symlink" || baseNameOf path != "result"))
        ./.;
      preBuild = buildExports;
      buildInputs = oldAttrs.buildInputs ++ deps;
  });

  drv = if static == true
    then drv2.overrideDerivation (oldAttrs: {
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--disable-shared"
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        # double-conversion temporary patch
        # This is required on nix-packages 24.05 until this patch is merged
        # https://github.com/NixOS/nixpkgs/pull/322738
        "--extra-lib-dirs=${pkgs.double-conversion.overrideAttrs(_: { cmakeFlags = [ ]; })}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ];
      })
    else drv2;


  env = haskellPackages.shellFor {
    packages = p: with p; [
      drv
    ];

    buildInputs = with haskellPackages; deps ++ [
      niv
      pkgs.cacert # needed for niv
      pkgs.nix    # needed for niv
      cabal-install
      pkgs.ghcid
      pkgs.cabal2nix
      # haskell-language-server
      # fast-tags
      # haskell-debug-adapter
      # ghci-dap
    ];

    withHoogle = withHoogle;

    shellHook = buildExports;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
