{ mkDerivation, base, containers, directory, fetchgit, filepath
, parsec, process, stdenv, strict, time
}:
mkDerivation {
  pname = "wxdirect";
  version = "0.93.0.0";
  src = fetchgit {
    url = "https://github.com/wxHaskell/wxHaskell.git";
    sha256 = "1ski5ig1l94h9aa34j12si1ilgrjsd7gvryvb71hh1l9lrdx5wy6";
    rev = "b7ac21d1dba48076dc7538d1967a14d59cfeb615";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wxdirect; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath parsec process strict time
  ];
  homepage = "https://wiki.haskell.org/WxHaskell";
  description = "helper tool for building wxHaskell";
  license = stdenv.lib.licenses.bsd3;
}
