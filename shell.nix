{ pkgs ? import <nixpkgs> {} }:
let
  myHaskell = (pkgs.haskellPackages.ghcWithHoogle (p: with p; [ 
    cabal-doctest 
    cabal-install 
    doctest 
    filepath
    hslogger 
    hspec 
    lsp 
    megaparsec 
    ordered-containers 
    ormolu 
  ]));
in
pkgs.mkShell {
  name = "dimensional";
  shellHook = ''
    export NIX_GHC=${myHaskell}/bin/ghc
    export NIX_GHC_LIBDIR=${myHaskell}/lib/ghc-8.10.7
  '';
  buildInputs = with pkgs; [ 
    myHaskell
    haskell-language-server
    coq
    nodejs
    yarn
  ];
}
