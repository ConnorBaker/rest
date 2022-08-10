{
  mkDerivation,
  aeson,
  base,
  bytestring,
  containers,
  hashable,
  lib,
  mtl,
  parsec,
  QuickCheck,
  rest-rewrite,
  stm,
  tagged,
  tasty,
  tasty-golden,
  tasty-hunit,
  tasty-json,
  tasty-quickcheck,
  text,
  unordered-containers,
}:
mkDerivation {
  pname = "rest-rewrite-test";
  version = "0.4.0";
  src = ../../rest-rewrite-test;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    containers
    hashable
    mtl
    parsec
    QuickCheck
    rest-rewrite
    stm
    tagged
    tasty
    tasty-golden
    tasty-hunit
    tasty-json
    tasty-quickcheck
    text
    unordered-containers
  ];
  description = "Rewriting library with online termination checking";
  license = lib.licenses.bsd3;
  mainProgram = "rest-rewrite-test";
}
