{ mkDerivation, base, filepath, optparse-applicative, process
, stdenv, template-haskell, temporary, unix, vcr
}:
mkDerivation {
  pname = "vcr-app";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base filepath optparse-applicative process template-haskell
    temporary unix vcr
  ];
  description = "event recorder";
  license = stdenv.lib.licenses.gpl3;
}
