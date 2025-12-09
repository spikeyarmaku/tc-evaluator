cmake_minimum_required(VERSION 3.10.0)
project(TreeCalculusEvaluator VERSION 0.1.0 LANGUAGES C DESCRIPTION "Tree Calculus evaluator library")

if(CMAKE_BUILD_TYPE STREQUAL Release)
    include(CheckIPOSupported)
    check_ipo_supported(RESULT IPO_SUPPORTED OUTPUT error)
    if (IPO_SUPPORTED)
        message(STATUS "IPO / LTO enabled")
        set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
    else()
        message(STATUS "IPO / LTO not supported: <${error}>")
    endif()
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")
if(CMAKE_EXPORT_COMPILE_COMMANDS)
  set(CMAKE_C_STANDARD_INCLUDE_DIRECTORIES
      ${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES})
endif()

set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

add_library(tceval
    "${SRC_DIR}/array.c"
    "${SRC_DIR}/debug.c"
    "${SRC_DIR}/node.c"
    "${SRC_DIR}/vm.c"
    "${SRC_DIR}/tree.c"
    "${SRC_DIR}/tceval.c"
)

add_executable(example EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_SOURCE_DIR}/examples/main.c")
target_link_libraries(example PRIVATE tceval)

add_executable(benchmark EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_SOURCE_DIR}/benchmark/benchmark.c")
target_link_libraries(benchmark PRIVATE tceval)

if(NOT DEFINED CMAKE_BUILD_TYPE)
    message(STATUS "No CMAKE_BUILD_TYPE set -- using Release")
    set(CMAKE_BUILD_TYPE Release)
endif(NOT DEFINED CMAKE_BUILD_TYPE)

if (CMAKE_BUILD_TYPE STREQUAL Profiling)
    message(STATUS "Building with profiling info")
    target_compile_options(tceval PRIVATE -O0 -g)
elseif(CMAKE_BUILD_TYPE STREQUAL Debug)
    message(STATUS "Building Debug version")
    target_compile_options(tceval PRIVATE -O0)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    message(STATUS "Building Release version")
    target_compile_options(tceval PRIVATE -O3)
endif()

# --- TESTS ---

enable_testing()
set(TEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/test")

add_executable(test_array "${TEST_DIR}/array.c")
target_link_libraries(test_array PRIVATE tceval)
add_test(NAME array COMMAND test_array)

add_executable(test_node "${TEST_DIR}/node.c")
target_link_libraries(test_node PRIVATE tceval)
add_test(NAME node COMMAND test_node)

add_executable(test_tree "${TEST_DIR}/tree.c")
target_link_libraries(test_tree PRIVATE tceval)
add_test(NAME tree COMMAND test_tree)

add_executable(test_vm "${TEST_DIR}/vm.c")
target_link_libraries(test_vm PRIVATE tceval)
add_test(NAME vm COMMAND test_vm)
