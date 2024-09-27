{ mkDerivation, base, base16-bytestring, base64-bytestring
, bytestring, containers, deepseq, deepseq-generics, fetchgit
, filepath, HUnit, lib, megaparsec, mtl, parser-combinators
, QuickCheck, test-framework, test-framework-hunit
, test-framework-quickcheck2, xml
}:
mkDerivation {
  pname = "deseo";
  version = "1.6.2";
  src = fetchgit {
    url = "https://github.com/zoranbosnjak/deseo";
    sha256 = "0b4lv1qnmmb3ia1a4bdhln6z4fkbzlr5p02cbbzb10ncr09d7w7i";
    rev = "985db88231dc971d1ce0289ae518b9c0fc9217cd";
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
  license = lib.licenses.gpl3Only;
}
