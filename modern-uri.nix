{ mkDerivation, base, bytestring, containers, contravariant
, criterion, deepseq, exceptions, fetchgit, hspec, hspec-discover
, hspec-megaparsec, megaparsec, mtl, profunctors, QuickCheck
, reflection, stdenv, tagged, template-haskell, text, weigh
}:
mkDerivation {
  pname = "modern-uri";
  version = "0.3.1.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/modern-uri.git";
    sha256 = "1abblbw53bqpx9zcrqy8g48izyikavr4w4jf91hq9j95jxkmw6r3";
    rev = "13bc7841cc270974c819ccb2ac900fabda6d7423";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring containers contravariant deepseq exceptions
    megaparsec mtl profunctors QuickCheck reflection tagged
    template-haskell text
  ];
  testHaskellDepends = [
    base bytestring hspec hspec-megaparsec megaparsec QuickCheck text
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    base bytestring criterion deepseq megaparsec text weigh
  ];
  homepage = "https://github.com/mrkkrp/modern-uri";
  description = "Modern library for working with URIs";
  license = stdenv.lib.licenses.bsd3;
}
