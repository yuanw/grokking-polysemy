{ mkDerivation
, base
, hpack
, persistent
, persistent-sqlite
, persistent-template
, polysemy
, polysemy-plugin
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
    persistent
    persistent-sqlite
    persistent-template
    polysemy
    polysemy-plugin
    random
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base
    persistent
    persistent-sqlite
    persistent-template
    polysemy
    polysemy-plugin
    random
  ];
  prePatch = "hpack";
  homepage = "https://github.com/yuanw/grokking-polysemy#readme";
  license = stdenv.lib.licenses.mit;
}
