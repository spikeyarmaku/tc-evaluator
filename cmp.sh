# Increment build number
BUILD_NUMBER_FILE="build_number.txt"
BUILD=$(($(cat $BUILD_NUMBER_FILE) + 1))
echo $BUILD > $BUILD_NUMBER_FILE
echo "Tree Calculus runtime - build $BUILD"

cabal run tc-comp -- test.txt -cb test.txt.c
# gcc test.txt.c -o .output/a.out -O3
gcc test.txt.c src/bintree/debug.c src/bintree/node.c \
    src/bintree/vm.c src/bintree/stack.c src/bintree/tree.c \
    -o .output/a.out -O3 -flto # -march=native -fno-plt -DNDEBUG