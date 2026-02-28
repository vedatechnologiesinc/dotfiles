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
    chibi
    chicken
    gauche
    racket
    scsh
    chez
    gerbil-unstable
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs;
}
