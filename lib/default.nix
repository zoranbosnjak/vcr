{ mkDerivation, aeson, aeson-pretty, async, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, deepseq, deseo
, directory, filepath, gi-gtk, haskell-gi-base, hslogger, hsyslog
, http-types, megaparsec, network, network-multicast
, optparse-applicative, process, QuickCheck, req, retry, stdenv
, stm, template-haskell, temporary, text, text-format, time
, transformers, unix, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "vcr";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty async base base16-bytestring base64-bytestring
    bytestring clock containers deepseq deseo directory filepath gi-gtk
    haskell-gi-base hslogger hsyslog http-types megaparsec network
    network-multicast optparse-applicative process QuickCheck req retry
    stm template-haskell temporary text text-format time transformers
    unix unordered-containers uuid wai warp
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
