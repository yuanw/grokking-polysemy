{ mkDerivation, base, polysemy, stdenv }:
mkDerivation {
  pname = "grokking-polysemy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base polysemy ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
