{ system ? builtins.currentSystem }:

with import <nixpkgs> { inherit system; };

mkShell {
  buildInputs = [
    haskell.compiler.ghc924
    cabal-install
    haskell-ci
  ];
}
