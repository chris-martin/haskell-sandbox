{ mkDerivation, base, checkers, QuickCheck, random, safe, stdenv }:
mkDerivation {
  pname = "sandbox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base checkers QuickCheck random safe
  ];
  license = stdenv.lib.licenses.asl20;
}
