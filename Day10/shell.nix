{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [ pkgs.swiProlog ];
    EPROLOG = pkgs.swiProlog;
}
