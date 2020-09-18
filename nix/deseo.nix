{ mkDerivation, base, base16-bytestring, base64-bytestring
, bytestring, containers, deepseq, deepseq-generics, fetchgit
, filepath, HUnit, megaparsec, mtl, parser-combinators, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, xml
}:
mkDerivation {
  pname = "deseo";
  version = "1.6.0";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/deseo";
    sha256 = "1yhqq6lv3z20bpxh77l7lgblycv8bn6bp8c9d6pdgf0qr868n4v6";
    rev = "ff9025864bd83d13e83453946053874a0ab6f41a";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring containers
    deepseq deepseq-generics megaparsec mtl parser-combinators
    QuickCheck xml
  ];
  testHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring containers
    filepath HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2
  ];
  description = "Asterix decoder/encoder";
  license = stdenv.lib.licenses.gpl3;
}
