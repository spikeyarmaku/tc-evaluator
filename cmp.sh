cabal run tc-comp -- test.txt -ci test.txt.c
clang test.txt.c -o .output/a.out -O3