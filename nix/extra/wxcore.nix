{ mkDerivation, array, base, bytestring, containers, directory
, fetchgit, filepath, lib, parsec, stm, time, wxc, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxcore";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://codeberg.org/wxHaskell/wxHaskell.git";
    sha256 = "0qggb7jy3mrg0yw5z1agz8wg7zh421fplhcla0ng73z3a667rc0z";
    rev = "08f96902abb53c6d1baed3e79dece2c83b8ab474";
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
