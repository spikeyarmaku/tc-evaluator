cabal run tc-comp -- test.txt -cb test.txt.c
gcc test.txt.c src/bintree/debug.c src/bintree/node.c \
    src/bintree/reduction.c src/bintree/stack.c src/bintree/tree.c \
    -o .output/a.out -O0 -g
# valgrind --tool=callgrind .output/a.out
valgrind --tool=massif .output/a.out