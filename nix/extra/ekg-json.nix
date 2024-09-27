{ mkDerivation, aeson, base, ekg-core, fetchgit, lib, text
, unordered-containers
}:
mkDerivation {
  pname = "ekg-json";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/L0neGamer/ekg-json";
    sha256 = "1qbnws7y6hc7w33m3zhr3yshha85frhja8sbnnsj3l7haw8wr19r";
    rev = "b56408fab35642b27b874cf17a522e6ff5154bd6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base ekg-core text unordered-containers
  ];
  homepage = "https://github.com/L0neGamer/ekg-json";
  description = "JSON encoding of ekg metrics";
  license = lib.licenses.bsd3;
}
