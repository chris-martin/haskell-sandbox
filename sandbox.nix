{ mkDerivation, base, checkers, QuickCheck, stdenv }:
mkDerivation {
  pname = "sandbox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base checkers QuickCheck ];
  license = stdenv.lib.licenses.asl20;
}
