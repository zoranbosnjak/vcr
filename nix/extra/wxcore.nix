{ mkDerivation, array, base, bytestring, containers, directory
, fetchgit, filepath, lib, parsec, stm, time, wxc, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxcore";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://codeberg.org/wxHaskell/wxHaskell.git";
    sha256 = "1hvzf9802wsgl3zyghjrk6wj78gw5s8g5zkjzqg7jpzcczvplpv6";
    rev = "5ce677420cbc49bc810ee075922732ed0f3e838c";
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
