{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, directory, ekg, exceptions, filepath, http-types, network
, optparse-applicative, pipes, pipes-concurrency, pipes-safe
, process, stdenv, stm, template-haskell, temporary, text, time
, transformers, unix, unliftio, uuid, vcr, wai, warp
}:
mkDerivation {
  pname = "vcr-app";
  version = "0.13.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring containers directory ekg
    exceptions filepath http-types network optparse-applicative pipes
    pipes-concurrency pipes-safe process stm template-haskell temporary
    text time transformers unix unliftio uuid vcr wai warp
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
