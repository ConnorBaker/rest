{
  z3Source,
  ghcName,
  composeManyExtensions,
}: let
  z3 = import ./z3.nix {inherit z3Source;};

  rest-rewrite = import ./haskell.nix {
    inherit ghcName;
    cabal2nixifiedDrvPath = ../packages/rest-rewrite.nix;
    extraHaskellOptions = final: prev: let
      inherit (prev.haskell.lib.compose) addExtraLibraries;
    in [(addExtraLibraries [prev.z3])];
  };

  rest-rewrite-test = import ./haskell.nix {
    inherit ghcName;
    cabal2nixifiedDrvPath = ../packages/rest-rewrite-test.nix;
    # cabal2nixifiedDrvArgs = {inherit rest-rewrite;};
    extraHaskellOptions = final: prev: let
      inherit (prev.haskell.lib.compose) overrideCabal addExtraLibraries;
    in [
      (addExtraLibraries [prev.z3 prev.makeWrapper])
      (overrideCabal (drv: {
        postInstall = ''
          wrapProgram $out/bin/${drv.pname} --prefix PATH : ${prev.lib.makeBinPath [prev.z3]}
        '';
      }))
    ];
  };

  default = composeManyExtensions [z3 rest-rewrite rest-rewrite-test];
in {
  inherit z3 rest-rewrite rest-rewrite-test default;
}
