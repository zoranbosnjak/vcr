{ mkDerivation, base, fetchgit, keera-hails-reactivevalues, stdenv
, wx, wxcore
}:
mkDerivation {
  pname = "keera-hails-reactive-wx";
  version = "0.7.0";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/keera-hails.git";
    sha256 = "08a50vfcl7cz49jhkhdxsp80h63w4w69h4zpmpaz52j26dkn2lpv";
    rev = "f372b5618bd135cdd7ffa05f2fb2539a64990779";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/keera-hails-reactive-wx; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base keera-hails-reactivevalues wx wxcore
  ];
  homepage = "http://www.keera.co.uk/blog/community/";
  description = "Haskell on Rails - Reactive Fields for WX widgets";
  license = stdenv.lib.licenses.bsd3;
}
