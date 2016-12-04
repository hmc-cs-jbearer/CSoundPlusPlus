cmake_minimum_required(VERSION 2.8.3)
project(CSound++)
include(ExternalProject)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

ExternalProject_Add(csound
  PREFIX tpc/csound
  GIT_REPOSITORY https://github.com/csound/csound.git
  BUILD_COMMAND make -j10
  INSTALL_COMMAND make install COMMAND ldconfig
)

include_directories(tpc/csound/src/csound/include)
include_directories(tpc/csound/src/csound/nacl)

# Builds a shared library containing extensions to the CSound language required to run CSound++
add_subdirectory(src/main/c/csoundpp)
