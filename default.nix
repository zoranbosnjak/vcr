{ mkDerivation, aeson, aeson-pretty, async, attoparsec, base
, base16-bytestring, base64-bytestring, binary, bytestring, cereal
, clock, containers, convertible, directory, ekg, filepath, HDBC
, HDBC-postgresql, HDBC-sqlite3, hslogger, hsyslog, http-client
, http-conduit, http-types, HUnit, network, network-multicast
, optparse-applicative, postgresql-simple, QuickCheck, scotty
, stdenv, stm, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, text-format, time, transformers
, unix, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "vcr";
  version = "0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async attoparsec base base16-bytestring
    base64-bytestring binary bytestring cereal clock containers
    convertible directory ekg filepath HDBC HDBC-postgresql
    HDBC-sqlite3 hslogger hsyslog http-client http-conduit http-types
    network network-multicast optparse-applicative postgresql-simple
    QuickCheck scotty stm text text-format time transformers unix
    unordered-containers uuid wai warp
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async attoparsec base base16-bytestring
    base64-bytestring binary bytestring cereal clock containers
    convertible directory ekg filepath HDBC HDBC-postgresql
    HDBC-sqlite3 hslogger hsyslog http-client http-conduit http-types
    network network-multicast optparse-applicative postgresql-simple
    QuickCheck scotty stm text text-format time transformers unix
    unordered-containers uuid wai warp
  ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring cereal
    containers directory HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 time
  ];
  license = stdenv.lib.licenses.gpl3;
}
