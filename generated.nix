{ mkDerivation, aeson, aeson-pretty, async, attoparsec, base
, base16-bytestring, base64-bytestring, binary, bytestring, cereal
, clock, containers, convertible, deepseq, dhall, directory, ekg
, filepath, hslogger, hsyslog, http-client, http-conduit
, http-types, hw-kafka-client, hzk, network, network-multicast
, optparse-applicative, pipes, pipes-aeson, pipes-attoparsec
, pipes-binary, pipes-bytestring, pipes-concurrency, pipes-group
, pipes-network, pipes-parse, pipes-postgresql-simple, pipes-safe
, pipes-wai, postgresql-simple, QuickCheck, scotty, sqlite-simple
, stdenv, stm, tasty, tasty-golden, tasty-hspec, tasty-hunit
, tasty-program, tasty-quickcheck, temporary, text, text-format
, time, transformers, unix, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "vcr";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async attoparsec base base16-bytestring
    base64-bytestring binary bytestring cereal clock containers
    convertible deepseq dhall directory ekg filepath hslogger hsyslog
    http-client http-conduit http-types hw-kafka-client hzk network
    network-multicast optparse-applicative pipes pipes-aeson
    pipes-attoparsec pipes-binary pipes-bytestring pipes-concurrency
    pipes-group pipes-network pipes-parse pipes-postgresql-simple
    pipes-safe pipes-wai postgresql-simple QuickCheck scotty
    sqlite-simple stm text text-format time transformers unix
    unordered-containers uuid wai warp
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async attoparsec base base16-bytestring
    base64-bytestring binary bytestring cereal clock containers
    convertible deepseq dhall directory ekg filepath hslogger hsyslog
    http-client http-conduit http-types hw-kafka-client hzk network
    network-multicast optparse-applicative pipes pipes-aeson
    pipes-attoparsec pipes-binary pipes-bytestring pipes-concurrency
    pipes-group pipes-network pipes-parse pipes-postgresql-simple
    pipes-safe pipes-wai postgresql-simple QuickCheck scotty
    sqlite-simple stm text text-format time transformers unix
    unordered-containers uuid wai warp
  ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring cereal
    containers directory pipes pipes-safe stm tasty tasty-golden
    tasty-hspec tasty-hunit tasty-program tasty-quickcheck temporary
    time
  ];
  license = stdenv.lib.licenses.gpl3;
}
