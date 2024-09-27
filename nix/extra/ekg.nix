{ mkDerivation, aeson, base, bytestring, ekg-core, ekg-json
, fetchgit, filepath, lib, network, snap-core, snap-server, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "ekg";
  version = "0.4.1.0";
  src = fetchgit {
    url = "https://github.com/l0negamer/ekg";
    sha256 = "1zmp1k3wv9mf5bgrnqxl0sxjfvnwjx3k99r1f6miinnmj8c2qw22";
    rev = "aee2d5d96573371cdfa9489777ead5db54319244";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core ekg-json filepath network snap-core
    snap-server text time transformers unordered-containers
  ];
  homepage = "https://github.com/l0negamer/ekg";
  description = "Remote monitoring of processes";
  license = lib.licenses.bsd3;
}
