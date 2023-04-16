{ mkDerivation, base, gloss, lib, linear, random }:
mkDerivation {
  pname = "molecularDynamics";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gloss linear random ];
  homepage = "https://github.com/mkDoku/molecularDynamics#readme";
  license = lib.licenses.bsd3;
  mainProgram = "molecularDynamics";
}
