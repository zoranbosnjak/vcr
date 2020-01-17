{ mkDerivation, aeson, aeson-pretty, async, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, filepath, hslogger, hsyslog, http-types, megaparsec
, network, network-multicast, optparse-applicative, pipes
, pipes-concurrency, pipes-safe, process, QuickCheck, regex-tdfa
, req, retry, stdenv, stm, template-haskell, temporary, text
, text-format, time, transformers, unix, unordered-containers, uuid
, wai, warp, wx
}:
mkDerivation {
  pname = "vcr";
  version = "0.0.0";
  src = ./lib;
  libraryHaskellDepends = [
    aeson aeson-pretty async base base16-bytestring base64-bytestring
    bytestring clock containers deepseq deseo directory filepath
    hslogger hsyslog http-types megaparsec network network-multicast
    optparse-applicative pipes pipes-concurrency pipes-safe process
    QuickCheck regex-tdfa req retry stm template-haskell temporary text
    text-format time transformers unix unordered-containers uuid wai
    warp wx
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
