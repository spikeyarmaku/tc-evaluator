gcc src/bintree/node.c src/bintree/test/node.c \
    src/bintree/array.c src/bintree/test/array.c \
    src/bintree/tree.c src/bintree/test/tree.c \
    src/bintree/vm.c src/bintree/test/vm.c \
    src/bintree/test/test.c src/bintree/debug.c -o test.a
./test.a
