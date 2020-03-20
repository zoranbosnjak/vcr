{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, containers, filepath, http-types, network, optparse-applicative
, pipes, pipes-concurrency, pipes-safe, process, regex-tdfa, retry
, stdenv, stm, template-haskell, temporary, text, time
, transformers, unix, uuid, vcr, wai, warp
}:
mkDerivation {
  pname = "vcr-app";
  version = "0.2.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring containers filepath
    http-types network optparse-applicative pipes pipes-concurrency
    pipes-safe process regex-tdfa retry stm template-haskell temporary
    text time transformers unix uuid vcr wai warp
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
