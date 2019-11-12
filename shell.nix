{ gitrev ? "#devel"
, compiler ? "ghc881" # "default"
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = import (builtins.fetchGit nixpkgs) { };
  project = import ./release.nix { inherit compiler; };

in
  pkgs.stdenv.mkDerivation {
    name = "vcr-environment";
    buildInputs = project.env.nativeBuildInputs ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.ghcid
    ];
    shellHook = ''
      export SW_VERSION="devel"
      export GIT_REV=${gitrev}
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
    '';
  }

