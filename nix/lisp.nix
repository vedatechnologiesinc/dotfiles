{
  nixpkgs,
  pkgs,
  addComPkgs,
  ...
}:
with pkgs;
let
  extraPkgs = [
    libGL
    libGLU
    meson
    glfw
    openssl
    openssl.dev
    openssl.out
  ];
  mainPkgs = [
    sbcl
    ecl
    cl-launch
    libfixposix
  ];
in
mkShell {
  buildInputs = addComPkgs mainPkgs ++ extraPkgs;
  LD_LIBRARY_PATH = lib.strings.makeLibraryPath extraPkgs;
}
