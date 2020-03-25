{ mkDerivation, base, bytestring, Cabal, directory, fetchgit
, filepath, libGL, libX11, process, split, stdenv, wxdirect, wxGTK
}:
mkDerivation {
  pname = "wxc";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://github.com/wxHaskell/wxHaskell.git";
    sha256 = "1ski5ig1l94h9aa34j12si1ilgrjsd7gvryvb71hh1l9lrdx5wy6";
    rev = "b7ac21d1dba48076dc7538d1967a14d59cfeb615";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wxc; echo source root reset to $sourceRoot";
  setupHaskellDepends = [
    base bytestring Cabal directory filepath process split
  ];
  libraryHaskellDepends = [ base split wxdirect ];
  librarySystemDepends = [ libGL libX11 ];
  libraryPkgconfigDepends = [ wxGTK ];
  doHaddock = false;
  postInstall = "cp -v dist/build/libwxc.so.0.93.0.0 $out/lib/libwxc.so";
  postPatch = "sed -i -e '/ldconfig inst_lib_dir/d' Setup.hs";
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "wxHaskell C++ wrapper";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
