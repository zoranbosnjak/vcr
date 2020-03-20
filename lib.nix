{ mkDerivation, aeson, aeson-pretty, async, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, filepath, hslogger, hsyslog, http-client, http-types
, megaparsec, modern-uri, network, network-multicast
, optparse-applicative, pipes, pipes-concurrency, pipes-safe
, process, QuickCheck, regex-tdfa, req, retry, stdenv, stm
, template-haskell, temporary, text, text-format, time
, transformers, unix, unordered-containers, uuid, wai, warp, wx
, wxcore
}:
mkDerivation {
  pname = "vcr";
  version = "0.0.0";
  src = ./lib;
  libraryHaskellDepends = [
    aeson aeson-pretty async base base16-bytestring base64-bytestring
    bytestring clock containers deepseq deseo directory filepath
    hslogger hsyslog http-client http-types megaparsec modern-uri
    network network-multicast optparse-applicative pipes
    pipes-concurrency pipes-safe process QuickCheck regex-tdfa req
    retry stm template-haskell temporary text text-format time
    transformers unix unordered-containers uuid wai warp wx wxcore
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
