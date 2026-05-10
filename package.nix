{
  niceHaskell,
  ...
}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "hetmanshell";
  compiler = "ghc912";
  developPackageArgs.overrides = _: _: {
  };
}
