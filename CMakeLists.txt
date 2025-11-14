cmake_minimum_required(VERSION 3.10.0)
project(TreeCalculusEvaluator VERSION 0.1.0 LANGUAGES C DESCRIPTION "Tree Calculus evaluator library")

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

if (CMAKE_BUILD_TYPE STREQUAL Profiling)
    target_compile_options(TreeCalculusRuntime PRIVATE -O0 -g)
elseif(CMAKE_BUILD_TYPE STREQUAL Release)
    target_compile_options(TreeCalculusRuntime PRIVATE -O3 -flto)
endif()
target_link_libraries(tceval_example PRIVATE tceval)
