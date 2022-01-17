sudo apt install -y ghc
sudo apt install -y cabal-install
sudo apt install -y haskell-stack
sudo apt update  -y
sudo apt upgrade -y

stack upgrade
cabal update

## rm -rf ~/.stack
## rm -rf pedant
git clone git@github.com:Hazelfire/pedant.git
cd pedant

stack setup
stack build
