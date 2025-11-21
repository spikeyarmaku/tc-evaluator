cmake_minimum_required(VERSION 3.10.0)
project(TreeCalculusEvaluator VERSION 0.1.0 LANGUAGES C DESCRIPTION "Tree Calculus evaluator library")

# This is required by Zed to recognize includes
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")
if(CMAKE_EXPORT_COMPILE_COMMANDS)
  set(CMAKE_C_STANDARD_INCLUDE_DIRECTORIES
      ${CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES})
endif()

set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

add_library(tceval
    ${SRC_DIR}/array.c
    ${SRC_DIR}/debug.c
    ${SRC_DIR}/node.c
    ${SRC_DIR}/vm.c
    ${SRC_DIR}/tree.c
    ${SRC_DIR}/tceval.c
)

set(EXAMPLES_DIR "${CMAKE_CURRENT_SOURCE_DIR}/examples")

add_executable(tceval_example EXCLUDE_FROM_ALL
    ${EXAMPLES_DIR}/main.c)

if(NOT DEFINED CMAKE_BUILD_TYPE)
    message(STATUS "No CMAKE_BUILD_TYPE set -- using Release")
    set(CMAKE_BUILD_TYPE Release)
endif(NOT DEFINED CMAKE_BUILD_TYPE)

if (CMAKE_BUILD_TYPE STREQUAL Profiling)
    message(STATUS "Building with profiling info")
    target_compile_options(TreeCalculusRuntime PRIVATE -O0 -g)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    message(STATUS "Building Release version")
    target_compile_options(TreeCalculusRuntime PRIVATE -O3 -flto)
endif()
target_link_libraries(tceval_example PRIVATE tceval)

# --- TESTS ---

enable_testing()
set(TEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/test")
# add_subdirectory(test)
add_executable(tceval_test_array ${TEST_DIR}/array.c)
target_link_libraries(tceval_test_array PRIVATE tceval)
add_test(NAME test_array COMMAND tceval_test_array)
add_executable(tceval_test_node ${TEST_DIR}/node.c)
target_link_libraries(tceval_test_node PRIVATE tceval)
add_test(NAME test_node COMMAND tceval_test_node)
add_executable(tceval_test_tree ${TEST_DIR}/tree.c)
target_link_libraries(tceval_test_tree PRIVATE tceval)
add_test(NAME test_tree COMMAND tceval_test_tree)
add_executable(tceval_test_vm ${TEST_DIR}/vm.c)
target_link_libraries(tceval_test_vm PRIVATE tceval)
add_test(NAME test_vm COMMAND tceval_test_vm)
