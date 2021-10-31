{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "dimensional";
  buildInputs = with pkgs; [ 
    (haskellPackages.ghcWithHoogle (p: with p; [ ordered-containers megaparsec ]))
    haskell-language-server
    nodejs
  ];
}
