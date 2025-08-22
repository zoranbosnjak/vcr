{ sources ? import ./nix/sources.nix
, inShell ? null
, withHoogle ? false
, gitrev ? "devel"
}:

let

  pkgs = import sources.nixpkgs {
    overlays = [ replaceGTK ];
  };

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
    overrides = self: super: rec {
      # haskellPackage1 = self.callPackage ./nix/myPackage1.nix { };
      # haskellPackage2 = self.callCabal2nix "name" "${path}/path" { };
      # ...

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
  };};

  drv1 = haskellPackages.callCabal2nix "vcr" ./. { };

  drv = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type:
          (type != "directory" || baseNameOf path != ".git")
          && (type != "symlink" || baseNameOf path != "result"))
        ./.;
      buildInputs = oldAttrs.buildInputs ++ deps;
      preBuild = ''
        export LC_ALL=C.UTF-8
        export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
        export GIT_REV=${gitrev}
      '';
  });

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

    shellHook = ''
      export LC_ALL=C.UTF-8
      export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
      export GIT_REV=${gitrev}
      vcr() { runhaskell -Wall -ilib -iapp app/Main.hs "$@"; }
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv
