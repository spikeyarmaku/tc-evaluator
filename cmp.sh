# Increment build number
BUILD_NUMBER_FILE="build_number.txt"
BUILD=$(($(cat $BUILD_NUMBER_FILE) + 1))
echo $BUILD > $BUILD_NUMBER_FILE
echo "Tree Calculus runtime - build $BUILD"

cabal run tc-comp -- test.txt -c test.txt.c
gcc test.txt.c -o a.out -O3 -flto
