{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, containers, directory, ekg, filepath, http-types, network
, optparse-applicative, pipes, pipes-concurrency, pipes-safe
, process, regex-tdfa, regex-tdfa-text, stdenv, stm
, template-haskell, temporary, text, time, transformers, unix, uuid
, vcr, wai, warp
}:
mkDerivation {
  pname = "vcr-app";
  version = "0.11.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring containers directory ekg
    filepath http-types network optparse-applicative pipes
    pipes-concurrency pipes-safe process regex-tdfa regex-tdfa-text stm
    template-haskell temporary text time transformers unix uuid vcr wai
    warp
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
