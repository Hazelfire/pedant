{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "dimensional";
  buildInputs = with pkgs; [ 
    (haskellPackages.ghcWithHoogle (p: with p; [ ordered-containers megaparsec lsp ormolu]))
    haskell-language-server
    nodejs
  ];
}
