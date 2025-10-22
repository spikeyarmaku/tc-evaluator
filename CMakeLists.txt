cmake_minimum_required(VERSION 3.10.0)
project(TreeCalculusRuntime VERSION 0.1.0 LANGUAGES C)

add_executable(TreeCalculusRuntime)

include(CTest)
enable_testing()

option(RUN_CABAL "Run cabal to regenerate the tree code" ON)
set(INPUT_FILE "tree.txt")
set(OUTPUT_FILE "tc.out")

# Step 1: Run cabal to generate init.c
if(RUN_CABAL)
    set(GENERATED_C_FILE "init.c")
    add_custom_command(
        OUTPUT ${GENERATED_C_FILE}
        COMMAND cabal run tc-comp -- ${INPUT_FILE} -cb src/bintree/generated/${GENERATED_C_FILE}
        COMMENT "Running cabal to generate src/bintree/generated/${GENERATED_C_FILE}"
        DEPENDS ${INPUT_FILE}
        VERBATIM
    )
endif()

# Step 2. Compile the C sources
if (CMAKE_BUILD_TYPE STREQUAL Profiling)
    target_compile_options(TreeCalculusRuntime PRIVATE -O0 -g)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    target_compile_options(TreeCalculusRuntime PRIVATE -O3 -flto)
endif()
target_sources(TreeCalculusRuntime
    PRIVATE
    src/bintree/array.c
    src/bintree/debug.c
    src/bintree/node.c
    src/bintree/vm.c
    src/bintree/tree.c
    src/bintree/generated/${GENERATED_C_FILE}
    src/bintree/main.c
)
