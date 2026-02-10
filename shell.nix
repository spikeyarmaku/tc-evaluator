{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # gcc
    gcc
    gdb
    # glibc

    valgrind
    kdePackages.kcachegrind

    clang
    cmake
  ];
}
