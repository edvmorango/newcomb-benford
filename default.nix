{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "newcomb-benford";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/newcomb-benford#readme";
  license = stdenv.lib.licenses.bsd3;
}
