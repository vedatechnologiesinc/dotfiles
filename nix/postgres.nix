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
    postgresql
    pgloader
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs;
  LD_LIBRARY_PATH = lib.strings.makeLibraryPath [ openssl ];
}
