cmake_minimum_required(VERSION 3.10.0)
project(TreeCalculusRuntime VERSION 0.1.0 LANGUAGES C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")
if(CMAKE_EXPORT_COMPILE_COMMANDS)
  set(CMAKE_C_STANDARD_INCLUDE_DIRECTORIES
      ${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES})
endif()

add_executable(TreeCalculusRuntime)

include(CTest)
enable_testing()

option(RUN_CABAL "Run cabal to regenerate the tree code" ON)
set(INPUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/tree.txt")

# Step 1: Run cabal to generate init.h
set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/bintree")
set(GENERATED_DIR "${CMAKE_CURRENT_BINARY_DIR}/src/bintree")
set(GENERATED_C_FILE "${GENERATED_DIR}/init.h")
if(RUN_CABAL)
    file(MAKE_DIRECTORY ${GENERATED_DIR})
    add_custom_command(
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OUTPUT ${GENERATED_C_FILE}
        COMMAND cabal run tc-comp -- ${INPUT_FILE} -cb ${GENERATED_C_FILE}
        COMMENT "Running cabal to generate ${GENERATED_C_FILE}"
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
target_include_directories(TreeCalculusRuntime PRIVATE ${GENERATED_DIR})
target_sources(TreeCalculusRuntime
    PRIVATE
    ${SRC_DIR}/array.c
    ${SRC_DIR}/debug.c
    ${SRC_DIR}/node.c
    ${SRC_DIR}/vm.c
    ${SRC_DIR}/tree.c
    ${SRC_DIR}/main.c
    ${GENERATED_C_FILE}
)
