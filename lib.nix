{ mkDerivation, aeson, aeson-pretty, async, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, ekg, filepath, hslogger, hsyslog, http-client
, http-client-tls, http-types, megaparsec, network
, network-multicast, optparse-applicative, pipes, pipes-concurrency
, pipes-safe, process, QuickCheck, regex-tdfa, regex-tdfa-text
, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, temporary, text, text-format, time
, transformers, unix, unordered-containers, uuid, vector, wai, warp
, wx, wxcore
}:
mkDerivation {
  pname = "vcr";
  version = "0.0.0";
  src = ./lib;
  libraryHaskellDepends = [
    aeson aeson-pretty async base base16-bytestring base64-bytestring
    bytestring clock containers deepseq deseo directory ekg filepath
    hslogger hsyslog http-client http-client-tls http-types megaparsec
    network network-multicast optparse-applicative pipes
    pipes-concurrency pipes-safe process QuickCheck regex-tdfa
    regex-tdfa-text stm template-haskell temporary text text-format
    time transformers unix unordered-containers uuid vector wai warp wx
    wxcore
  ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
