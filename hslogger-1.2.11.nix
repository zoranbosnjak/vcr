{ mkDerivation, base, containers, directory, fetchgit, mtl, network
, old-locale, process, stdenv, time, unix
}:
mkDerivation {
  pname = "hslogger";
  version = "1.2.11";
  src = fetchgit {
    url = "https://github.com/jgoerzen/hslogger.git";
    sha256 = "0wrwf1j8n9r48whscr86dbdhrvlbwl9nqy5l52c56snvzqnxhg1l";
    rev = "1db5fdcb4ef503f79f959d9a9de979bc0364e640";
  };
  libraryHaskellDepends = [
    base containers directory mtl network old-locale process time unix
  ];
  homepage = "http://software.complete.org/hslogger";
  description = "Versatile logging framework";
  license = stdenv.lib.licenses.bsd3;
}
