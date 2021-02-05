{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, ekg, exceptions, filepath, hslogger, hsyslog
, http-client, http-client-tls, http-types, megaparsec, network
, network-multicast, optparse-applicative, pipes, pipes-concurrency
, pipes-safe, process, QuickCheck, random, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, temporary, text
, text-format, time, transformers, unix, unliftio
, unordered-containers, uuid, vector, wai, warp, wx, wxcore
}:
mkDerivation {
  pname = "vcr";
  version = "0.0.0";
  src = ./lib;
  libraryHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers deepseq deseo directory ekg exceptions
    filepath hslogger hsyslog http-client http-client-tls http-types
    megaparsec network network-multicast optparse-applicative pipes
    pipes-concurrency pipes-safe process QuickCheck stm
    template-haskell temporary text text-format time transformers unix
    unliftio unordered-containers uuid vector wai warp wx wxcore
  ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    exceptions filepath pipes pipes-concurrency pipes-safe random tasty
    tasty-hunit tasty-quickcheck temporary text time unliftio
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
