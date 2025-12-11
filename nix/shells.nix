{
  nixpkgs,
  pkgs,
  ...
}:
with pkgs;
let
  comPkgs = [
    which
    rlwrap
    getopt
    curl
    glib
    zlib
  ];
  addComPkgs = l: l ++ comPkgs;
  inherits = {
    inherit nixpkgs pkgs addComPkgs;
  };
  imp = mod: import mod inherits;
in
rec {
  lisp = imp ./lisp.nix;
  haskell = imp ./haskell.nix;
  scheme = imp ./scheme.nix;
  clojure = imp ./clojure.nix;
  postgres = imp ./postgres.nix;
  www = imp ./www.nix;
  default = lisp;
}
