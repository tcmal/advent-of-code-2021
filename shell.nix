{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
  		emacs
	ghc
      racket
      clojure
      leiningen
	];
}
