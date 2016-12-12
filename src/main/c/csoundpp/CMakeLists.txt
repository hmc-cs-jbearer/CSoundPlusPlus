cmake_minimum_required(VERSION 2.8.3)

set (CMAKE_C_FLAGS "--std=c99 ${CMAKE_C_FLAGS}")

file(GLOB_RECURSE LIB_SRC "opcodes/*.c")

add_library(csoundpp SHARED ${LIB_SRC})
add_dependencies(csoundpp csound)
