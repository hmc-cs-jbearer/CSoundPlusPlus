cmake_minimum_required(VERSION 2.8.3)

file(GLOB_RECURSE LIB_SRC "opcodes/*.c")

add_library(csoundpp SHARED ${LIB_SRC})
