{ mkDerivation, aeson, authenticate-oauth, base, blaze-builder
, bytestring, case-insensitive, connection, fetchgit, hspec
, hspec-core, hspec-discover, http-api-data, http-client
, http-client-tls, http-types, modern-uri, monad-control, mtl
, QuickCheck, retry, stdenv, text, time, transformers
, transformers-base, unordered-containers
}:
mkDerivation {
  pname = "req";
  version = "3.0.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/req.git";
    sha256 = "1fyi9ncifi77dlafwx6bh2n448mbjy0agfpiiiw6wxxs1yv3i7qf";
    rev = "178e12f80bacc105a7f5898ddd0f96dbce486f80";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson authenticate-oauth base blaze-builder bytestring
    case-insensitive connection http-api-data http-client
    http-client-tls http-types modern-uri monad-control mtl retry text
    time transformers transformers-base
  ];
  testHaskellDepends = [
    aeson base blaze-builder bytestring case-insensitive hspec
    hspec-core http-client http-types modern-uri monad-control mtl
    QuickCheck retry text time unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  doCheck = false;
  homepage = "https://github.com/mrkkrp/req";
  description = "Easy-to-use, type-safe, expandable, high-level HTTP client library";
  license = stdenv.lib.licenses.bsd3;
}
