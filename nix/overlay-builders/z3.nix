{z3Source}: final: prev: let
  inherit (prev.pkgs.lib) importJSON lists attrsets hasPrefix removePrefix;
  z3CommitToTag = importJSON ../sources/z3-commit-to-tag.json;
  z3Rev = z3Source.rev;
  z3Tag =
    if lists.elem z3Rev (attrsets.attrNames z3CommitToTag)
    then z3CommitToTag.${z3Rev}
    else builtins.abort "Z3 commit ${z3Rev} is unsupported because it does not correspond to a known release tag";
  z3Version =
    if hasPrefix "z3-" z3Tag
    then removePrefix "z3-" z3Tag
    else if hasPrefix "Z3-" z3Tag
    then removePrefix "Z3-" z3Tag
    else builtins.abort "Z3 tag tied to ${z3Rev} is unsupported because it does not start with 'z3-' or 'Z3-'";
in {
  z3 = prev.z3.overrideAttrs (_: {
    version = z3Version;
    src = z3Source;
    doCheck = false;
    pythonBindings = false;
  });
}
