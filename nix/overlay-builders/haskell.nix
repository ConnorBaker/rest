{
  ghcName,
  cabal2nixifiedDrvPath,
  cabal2nixifiedDrvArgs ? {},
  extraHaskellOptions ? _: _: [],
}: final: prev: let
  haskellPkgs = prev.haskell.packages.${ghcName};
  inherit (prev.lib) pipe recursiveUpdateUntil;
  inherit (prev.haskell.lib.compose) dontCheck dontHaddock;

  drv = pipe (haskellPkgs.callPackage cabal2nixifiedDrvPath cabal2nixifiedDrvArgs) ([
      dontHaddock
      dontCheck
    ]
    ++ (extraHaskellOptions final prev));

  new = {
    haskell.packages.${ghcName} = haskellPkgs.extend (_: _: {
      "${drv.pname}" = drv;
    });
  };
in
  recursiveUpdateUntil (path: l: r: path == ["haskell" "packages" ghcName]) prev new
