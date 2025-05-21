gcc -g -O0 test.txt.c src/bintree/debug.c src/bintree/node.c \
    src/bintree/vm.c src/bintree/stack.c src/bintree/tree.c \
    -o .output/a.out

gdb .output/a.out
