{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
  		emacs
      racket
      clojure
      leiningen
	];
}
