{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    # gcc
    pkgs.gcc
    pkgs.gdb
    # pkgs.glibc

    # haskell
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.cabal-install

    pkgs.valgrind
    pkgs.libsForQt5.kcachegrind

    pkgs.clang
    pkgs.cmake
  ];
}
