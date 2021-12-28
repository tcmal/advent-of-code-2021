{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
  		emacs
	    (haskellPackages.ghcWithPackages (p: [
          p.linear
          p.parsec
      ]))
      stack
      racket
      clojure
      leiningen
	];
}
