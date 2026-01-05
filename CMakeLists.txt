cmake_minimum_required(VERSION 3.10.0)
project(Shrubble VERSION 0.1.0 LANGUAGES C DESCRIPTION "Shrubble - a VM for evaluating Tree Calculus terms")

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

add_library(shrubble
    "${SRC_DIR}/array.c"
    "${SRC_DIR}/debug.c"
    "${SRC_DIR}/node.c"
    "${SRC_DIR}/vm.c"
    "${SRC_DIR}/tree.c"
    "${SRC_DIR}/shrubble.c"
)

add_executable(example EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_SOURCE_DIR}/examples/main.c")
target_link_libraries(example PRIVATE shrubble)

add_executable(benchmark EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_SOURCE_DIR}/benchmark/benchmark.c")
target_link_libraries(benchmark PRIVATE shrubble)

if(NOT DEFINED CMAKE_BUILD_TYPE)
    message(STATUS "No CMAKE_BUILD_TYPE set -- using Release")
    set(CMAKE_BUILD_TYPE Release)
endif(NOT DEFINED CMAKE_BUILD_TYPE)

if (CMAKE_BUILD_TYPE STREQUAL Debug)
    message(STATUS "Building with debug info")
    target_compile_options(shrubble PRIVATE -O0 -g)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    message(STATUS "Building Release version")
    target_compile_options(shrubble PRIVATE -O3)
else()
    message(STATUS "Invalid build type ${CMAKE_BUILD_TYPE}")
endif()

# --- TESTS ---

enable_testing()
set(TEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/test")

add_executable(test_array "${TEST_DIR}/array.c")
target_link_libraries(test_array PRIVATE shrubble)
add_test(NAME array COMMAND test_array)

add_executable(test_node "${TEST_DIR}/node.c")
target_link_libraries(test_node PRIVATE shrubble)
add_test(NAME node COMMAND test_node)

add_executable(test_tree "${TEST_DIR}/tree.c")
target_link_libraries(test_tree PRIVATE shrubble)
add_test(NAME tree COMMAND test_tree)

add_executable(test_vm "${TEST_DIR}/vm.c")
target_link_libraries(test_vm PRIVATE shrubble)
add_test(NAME vm COMMAND test_vm)
