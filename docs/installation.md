# Installing pedant

So far, this project has only ever been compiled on Linux systems.
However, if you are on Windows or Mac, there is no particular reason that
this project would be difficult to build, as it only uses a standard Haskell stack to compile.

## Nix

The main developer uses a nix based system.

Provided with this project is a `shell.nix`, running `nix-shell` in the
root directory of this project should give you all the tools you need to
compile. All you need to do after this is to:

```
cabal build
cabal install
```

to install pedant as a command line tool. Remember to add `~/.cabal/bin` to your `$PATH` to get access to the tool.

## Ubuntu and Debian

There is a `make-ubuntu.bash` in the main directory of the project that
you can use to build this project, or guide you through the steps
required.

## Other systems

To install this, you will need
[Stack](https://docs.haskellstack.org/en/stable/README/) and
[Cabal-install](https://www.haskell.org/cabal/). Following similar
instructions in `make-ubuntu.bash` should help you build pedant. 
