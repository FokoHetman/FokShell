{
  niceHaskell,
  configuration,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = configuration;
  compiler = "ghc912";
  developPackageArgs.overrides = _: _: {
  };
}
