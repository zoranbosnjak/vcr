{ mkDerivation, array, base, bytestring, containers, directory
, fetchgit, filepath, lib, parsec, stm, time, wxc, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxcore";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://codeberg.org/wxHaskell/wxHaskell";
    sha256 = "1xh2lyz68w0yy3wk9qgf4dklrvgy3dvrhb7wby71ff4avvnf5ps3";
    rev = "cbcb56f1b3d49fbea716902994fe7c73619a5582";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wxcore; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base bytestring containers directory filepath parsec stm time
  ];
  librarySystemDepends = [ wxc ];
  libraryPkgconfigDepends = [ wxc wxGTK ];
  libraryToolDepends = [ wxdirect ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell core";
  license = "unknown";
  __propagatePkgConfigDepends = false;
}
