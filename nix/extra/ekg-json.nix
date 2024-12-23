{ mkDerivation, aeson, base, ekg-core, fetchgit, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "ekg-json";
  version = "0.1.1.1";
  src = fetchgit {
    url = "https://github.com/L0neGamer/ekg-json";
    sha256 = "1r3gdaxd9hffx8bbv68aarr4sr2ifmz2w8l74mfp2b53c8wsppbd";
    rev = "bd0592818882f9cf34d2991d01f7dcb3d8bca309";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base ekg-core text unordered-containers
  ];
  homepage = "https://github.com/L0neGamer/ekg-json";
  description = "JSON encoding of ekg metrics";
  license = lib.licenses.bsd3;
}
