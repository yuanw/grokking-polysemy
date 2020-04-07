{ mkDerivation, base, polysemy, polysemy-plugin, stdenv }:
mkDerivation {
  pname = "grokking-polysemy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base polysemy polysemy-plugin ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
