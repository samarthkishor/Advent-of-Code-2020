{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.swiProlog
    pkgs.clojure
    pkgs.leiningen
    pkgs.clj-kondo
    pkgs.jdk8

    # keep this line if you use bash
    pkgs.bashInteractive
  ];

  shellHook = ''
    export JAVA_HOME=${pkgs.jdk8}
    export PATH="${pkgs.jdk8}/bin:$PATH"
  '';
}
