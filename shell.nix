{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "dimensional";
  buildInputs = with pkgs; [ 
    (haskellPackages.ghcWithHoogle (p: with p; [ ordered-containers lsp megaparsec ormolu stack cabal-install ]))
    haskell-language-server
    coq
    nodejs
  ];
}
