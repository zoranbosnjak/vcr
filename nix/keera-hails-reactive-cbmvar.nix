{ mkDerivation, base, directory, fetchgit, filepath, hlint
, keera-callbacks, keera-hails-reactivevalues, lens, process
, regex-posix, stdenv
}:
mkDerivation {
  pname = "keera-hails-reactive-cbmvar";
  version = "0.7.0";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/keera-hails.git";
    sha256 = "08a50vfcl7cz49jhkhdxsp80h63w4w69h4zpmpaz52j26dkn2lpv";
    rev = "f372b5618bd135cdd7ffa05f2fb2539a64990779";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/keera-hails-reactive-cbmvar; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base keera-callbacks keera-hails-reactivevalues lens
  ];
  testHaskellDepends = [
    base directory filepath hlint process regex-posix
  ];
  homepage = "https://keera.co.uk/";
  description = "Reactive Haskell on Rails - CBMVars as reactive values";
  license = stdenv.lib.licenses.bsd3;
}
