{ mkDerivation, aeson, aeson-pretty, async, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, ekg, filepath, hslogger, hsyslog, http-client
, http-client-tls, http-types, megaparsec, modern-uri, network
, network-multicast, optparse-applicative, pipes, pipes-concurrency
, pipes-safe, process, QuickCheck, regex-tdfa, retry, stdenv, stm
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
    modern-uri network network-multicast optparse-applicative pipes
    pipes-concurrency pipes-safe process QuickCheck regex-tdfa retry
    stm template-haskell temporary text text-format time transformers
    unix unordered-containers uuid vector wai warp wx wxcore
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
