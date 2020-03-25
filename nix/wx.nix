{ mkDerivation, base, fetchgit, stdenv, stm, time, wxcore }:
mkDerivation {
  pname = "wx";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://github.com/wxHaskell/wxHaskell.git";
    sha256 = "1ski5ig1l94h9aa34j12si1ilgrjsd7gvryvb71hh1l9lrdx5wy6";
    rev = "b7ac21d1dba48076dc7538d1967a14d59cfeb615";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wx; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base stm time wxcore ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
