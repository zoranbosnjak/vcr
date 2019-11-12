{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, crypto-pubkey-types, data-default, fetchgit, http-client
, http-types, random, RSA, SHA, stdenv, time, transformers
, transformers-compat
}:
mkDerivation {
  pname = "authenticate-oauth";
  version = "1.6.0.1";
  src = fetchgit {
    url = "https://github.com/yesodweb/authenticate.git";
    sha256 = "0fnl52nh1kdf6bbmr6fi1ab0mfi1k5jnymmwx6qcpknlzanfm4wl";
    rev = "c390b11a24bc1186d765dd0a1228cc36d07fb9b0";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/authenticate-oauth; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base base64-bytestring blaze-builder bytestring crypto-pubkey-types
    data-default http-client http-types random RSA SHA time
    transformers transformers-compat
  ];
  homepage = "http://github.com/yesodweb/authenticate";
  description = "Library to authenticate with OAuth for Haskell web applications";
  license = stdenv.lib.licenses.bsd3;
}
