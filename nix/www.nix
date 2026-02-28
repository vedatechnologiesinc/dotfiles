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
    gnumake
    cl-launch
    emem
    parallel
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs ++ extraPkgs;
}
