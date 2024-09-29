{ sources ? import ./nix/sources.nix
, inShell ? null
, withHoogle ? false
, strip ? true
, static ? false    # build static binary
, gitrev ? "devel"
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

  wxHaskellRef = builtins.fromJSON (builtins.readFile ./nix/extra/wxHaskell.json);
  wxHaskellDir = pkgs.fetchgit {
    url = wxHaskellRef.url;
    rev = wxHaskellRef.rev;
    sha256 = wxHaskellRef.sha256;
  };

  khRef = builtins.fromJSON (builtins.readFile ./nix/extra/keera-hails.json);
  khDir = pkgs.fetchgit {
    url = khRef.url;
    rev = khRef.rev;
    sha256 = khRef.sha256;
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
        buildHaskellPackages = super.buildHaskellPackages.override
          (oldBuildHaskellPackages: {
          ghc = fixGHC oldBuildHaskellPackages.ghc;
        });
        # haskellPackage1 = self.callPackage ./nix/myPackage1.nix { };
        # haskellPackage2 = self.callCabal2nix "name" "${path}/path" { };
        # ...

        ekg = self.callPackage ./nix/extra/ekg.nix { };
        ekg-json = self.callPackage ./nix/extra/ekg-json.nix { };

        wx = self.callCabal2nix "wx" "${wxHaskellDir}/wx" { };
        wxcore = self.callPackage ./nix/extra/wxcore.nix { };
        wxdirect = self.callCabal2nix "wxdirect" "${wxHaskellDir}/wxdirect" { };
        wxc = self.callPackage "${wxHaskellDir}/wxc/package.nix" { };

        keera-hails-reactivevalues = self.callCabal2nix
          "keera-hails-reactivevalues" "${khDir}/keera-hails-reactivevalues" { };
        keera-hails-reactive-cbmvar = self.callCabal2nix
          "keera-hails-reactive-cbmvar" "${khDir}/keera-hails-reactive-cbmvar" { };
        keera-hails-reactive-wx = self.callCabal2nix
          "keera-hails-reactive-wx" "${khDir}/keera-hails-reactive-wx" { };
        keera-hails-reactive-mvc-view = self.callCabal2nix
          "keera-hails-reactive-mvc-view" "${khDir}/keera-hails-reactive-mvc-view" { };

        deseo = self.callPackage ./nix/extra/deseo.nix { };
  };};

  preHook = ''
    export LC_ALL=C.UTF-8
    export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
    export GIT_REV=${gitrev}
    export GHC_BASE=$(which ghc | cut -d '/' -f-4)
    export WXC_LIB=${haskellPackages.wxc}/lib
  '';

  drv1 = haskellPackages.callCabal2nix "vcr" ./. { };

  drv2 = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type:
          (type != "directory" || baseNameOf path != ".git")
          && (type != "symlink" || baseNameOf path != "result"))
        ./.;
      preBuild = preHook;
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
      hlint
      stylish-haskell
      # haskell-language-server
      # fast-tags
      # haskell-debug-adapter
      # ghci-dap
    ];

    withHoogle = withHoogle;

    shellHook = preHook;
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
