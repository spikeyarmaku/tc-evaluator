cabal run tc-comp -- test.txt -cb test.txt.c
gcc -g -O0 test.txt.c src/bintree/debug.c src/bintree/node.c \
    src/bintree/reduction.c src/bintree/stack.c src/bintree/tree.c \
    -o .output/a.out
valgrind --track-origins=yes .output/a.out