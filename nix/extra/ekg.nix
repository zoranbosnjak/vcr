{ mkDerivation, aeson, base, bytestring, ekg-core, ekg-json
, fetchgit, filepath, lib, network, snap-core, snap-server, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "ekg";
  version = "0.4.1.1";
  src = fetchgit {
    url = "https://github.com/l0negamer/ekg";
    sha256 = "1czm3lzdlph02zy1yg2hmcl91k3zs44b9rgvzjszk1sfjb6r775w";
    rev = "15220cdf0c5756045059c75e9d16717c244a357f";
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
