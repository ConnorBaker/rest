{
  mkDerivation,
  base,
  containers,
  hashable,
  lib,
  mtl,
  parsec,
  process,
  text,
  unordered-containers,
}:
mkDerivation {
  pname = "rest-rewrite";
  version = "0.4.0";
  src = ../../rest-rewrite;
  libraryHaskellDepends = [
    base
    containers
    hashable
    mtl
    parsec
    process
    text
    unordered-containers
  ];
  description = "Rewriting library with online termination checking";
  license = lib.licenses.bsd3;
}
