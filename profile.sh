cmake -B build -DCMAKE_BUILD_TYPE=Profiling
cmake --build build --target=benchmark

# Memcheck:
# valgrind --track-origins=yes build/benchmark

# Callgraph:
valgrind --tool=callgrind --collect-jumps=yes build/benchmark

# Heap profiling:
# valgrind --tool=massif build/benchmark
