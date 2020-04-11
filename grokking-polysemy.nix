{ mkDerivation
, base
, hpack
, polysemy
, polysemy-plugin
, postgresql-simple
, random
, stdenv
}:
mkDerivation {
  pname = "grokking-polysemy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    polysemy
    polysemy-plugin
    postgresql-simple
    random
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base
    polysemy
    polysemy-plugin
    postgresql-simple
    random
  ];
  prePatch = "hpack";
  homepage = "https://github.com/yuanw/grokking-polysemy#readme";
  license = stdenv.lib.licenses.mit;
}
