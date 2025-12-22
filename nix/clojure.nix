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
  mainPkgs = [
    leiningen
    babashka
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs ++ extraPkgs;
}
