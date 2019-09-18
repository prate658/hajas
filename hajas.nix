{ mkDerivation, base, containers, hpack, language-ecmascript, mtl
, optparse-applicative, split, stdenv, tasty, tasty-hunit, uniplate
}:
mkDerivation {
  pname = "hajas";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers language-ecmascript mtl split uniplate
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers language-ecmascript mtl optparse-applicative split
    uniplate
  ];
  testHaskellDepends = [
    base containers language-ecmascript mtl split tasty tasty-hunit
    uniplate
  ];
  doCheck = false;
  preConfigure = "hpack";
  homepage = "https://github.com/prate658/hajas#readme";
  license = stdenv.lib.licenses.bsd3;
}
