{
  nixpkgs,
  pkgs,
  addComPkgs,
  ...
}:
with pkgs;
let
  extraPkgs = [
  ];
  haskellPkgs = haskell.packages."ghc9103";
  stackWrapped = symlinkJoin {
    name = "stack";
    paths = [ stack ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --system-ghc \
          --no-install-ghc \
        "
    '';
  };
  mainPkgs = [
    haskellPkgs.ghc
    haskellPkgs.haskell-language-server
    stackWrapped
    fourmolu
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs ++ extraPkgs;
  LD_LIBRARY_PATH = lib.makeLibraryPath mainPkgs;
}
