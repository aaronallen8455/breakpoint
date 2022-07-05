{ system ? builtins.currentSystem }:

with import <nixpkgs> { inherit system; };

mkShell {
  buildInputs = [
    haskell.compiler.ghc923
    cabal-install
  ];
}
