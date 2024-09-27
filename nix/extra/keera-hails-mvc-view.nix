{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "keera-hails-mvc-view";
  version = "0.7.0";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/keera-hails.git";
    sha256 = "08a50vfcl7cz49jhkhdxsp80h63w4w69h4zpmpaz52j26dkn2lpv";
    rev = "f372b5618bd135cdd7ffa05f2fb2539a64990779";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/keera-hails-mvc-view; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ];
  homepage = "http://www.keera.co.uk/blog/community/";
  description = "Haskell on Gtk rails - Generic View for MVC applications";
  license = stdenv.lib.licenses.bsd3;
}
