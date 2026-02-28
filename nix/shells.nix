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
  import' = m: import m inherits;
in
rec {
  lisp = import' ./lisp.nix;
  haskell = import' ./haskell.nix;
  scheme = import' ./scheme.nix;
  clojure = import' ./clojure.nix;
  postgres = import' ./postgres.nix;
  www = import' ./www.nix;
  default = lisp;
}
