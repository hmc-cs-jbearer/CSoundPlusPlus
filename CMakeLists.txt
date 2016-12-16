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

include_directories(tpc/csound/src/csound/include)
include_directories(tpc/csound/src/csound/nacl)

# Builds a shared library containing extensions to the CSound language required to run CSound++
add_subdirectory(src/main/c/csoundpp)

# sbt setup
if ("$ENV{SCALA_VERSION}")
  set(SBT_SCALA_VERSION "++$ENV{SCALA_VERSION}")
  message("Using scala version ${SBT_SCALA_VERSION}.")
else()
  set(SBT_SCALA_VERSION "")
endif()

file(GLOB_RECURSE SCALA_SRC src/main/scala/*.scala)

# Build a fat jar with the cspp language
add_custom_command(
  OUTPUT bin/cspp.jar
  COMMAND sbt "${SBT_SCALA_VERSION}" assembly
  COMMAND cmake -E touch bin/cspp.jar # It's possible that sbt determined that the jar was already
                                      # up to date. Touch it to tell cmake the dependency is satisfied
  DEPENDS ${SCALA_SRC} build.sbt project/plugins.sbt
  COMMENT "Building cspp.jar"
)
add_custom_target(jar ALL DEPENDS bin/cspp.jar)

# Test runners
add_custom_target(unit-test sbt ${SBT_SCALA_VERSION} test COMMENT "Running unit tests")
add_custom_target(unit-test-coverage sbt ${SBT_SCALA_VERSION} coverage test
  COMMENT "Running unit tests with coverage reporting")
add_custom_target(integration-test python3 -m nose -vv src/test/python/csoundpp DEPENDS jar csppstd
  COMMENT "Running integration tests")
add_custom_target(test-all DEPENDS unit-test integration-test)
add_custom_target(test-all-coverage DEPENDS unit-test-coverage integration-test)
add_custom_target(coverage-report sbt coverageReport coveralls
    COMMENT "Uploading coverage report")
