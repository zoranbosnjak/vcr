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
    sha256 = "0f0gswvmi70yyb66br544l3m08wxdq3bvf5hc110341cxk3hwq6a";
    rev = "b124b9e6a01d59990fee6c24fbc7d44579ad71a9";
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
