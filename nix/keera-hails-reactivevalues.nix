{ mkDerivation, base, cabal-install, contravariant, directory
, fetchgit, filepath, hlint, HUnit, mtl, process, QuickCheck
, regex-posix, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "keera-hails-reactivevalues";
  version = "0.7.0";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/keera-hails.git";
    sha256 = "08a50vfcl7cz49jhkhdxsp80h63w4w69h4zpmpaz52j26dkn2lpv";
    rev = "f372b5618bd135cdd7ffa05f2fb2539a64990779";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/keera-hails-reactivevalues; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base contravariant ];
  testHaskellDepends = [
    base directory filepath hlint HUnit mtl process QuickCheck
    regex-posix tasty tasty-hunit tasty-quickcheck
  ];
  testToolDepends = [ cabal-install ];
  homepage = "http://www.keera.co.uk/blog/community/";
  description = "Haskell on Rails - Reactive Values";
  license = stdenv.lib.licenses.bsd3;
}
