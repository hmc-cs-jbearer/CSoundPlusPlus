cmake_minimum_required(VERSION 2.8.3)
project(CSound++)
include(ExternalProject)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

# Download the headers for CSound 6.04.0. Our plugin will be supported by any version of CSound at
# least that recent. Currently, CSound 6.04.0 is what you get if you apt-get install csound, so
# it's a reasonable minimum version to support.
ExternalProject_Add(csound
  PREFIX tpc/csound
  GIT_REPOSITORY https://github.com/csound/csound.git
  GIT_TAG 6.04.0

  # Skip build and install, we just need headers
  CONFIGURE_COMMAND :
  BUILD_COMMAND :
  INSTALL_COMMAND :
)

# Build a fat jar with the cspp language
add_custom_target(cspp ALL
  COMMAND sbt assembly
  COMMENT "Building cspp.jar"
)

include_directories(tpc/csound/src/csound/include)
include_directories(tpc/csound/src/csound/nacl)

# Builds a shared library containing extensions to the CSound language required to run CSound++
add_subdirectory(src/main/c/csoundpp)
