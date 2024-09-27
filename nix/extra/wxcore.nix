{ mkDerivation, array, base, bytestring, Cabal, containers
, directory, fetchgit, filepath, parsec, process, stdenv, stm, time
, wxc, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxcore";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://github.com/wxHaskell/wxHaskell.git";
    sha256 = "1ski5ig1l94h9aa34j12si1ilgrjsd7gvryvb71hh1l9lrdx5wy6";
    rev = "b7ac21d1dba48076dc7538d1967a14d59cfeb615";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wxcore; echo source root reset to $sourceRoot";
  setupHaskellDepends = [ base Cabal directory filepath process ];
  libraryHaskellDepends = [
    array base bytestring containers directory filepath parsec stm time
    wxc wxdirect
  ];
  libraryPkgconfigDepends = [ wxGTK ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell core";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
