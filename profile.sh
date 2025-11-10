cmake -B build -DCMAKE_BUILD_TYPE=Profiling -DRUN_CABAL=on
cmake --build build

# Memcheck:
# valgrind --track-origins=yes build/TreeCalculusRuntime

# Callgraph:
valgrind --tool=callgrind --collect-jumps=yes build/TreeCalculusRuntime

# Heap profiling:
# valgrind --tool=massif build/TreeCalculusRuntime
