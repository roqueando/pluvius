cmake_minimum_required(VERSION 3.22)

set(PLUVIUS_PROJECT_NAME pluvius)
set(PLUVIUS_SOURCE_DIR src)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(PLUVIUS_PROJECT_VERSION 0.1)
set(PLUVIUS_LIBRARY_NAME pluvius)

include(cmake/UniversalAppleBuild.cmake)

project(
  ${PLUVIUS_PROJECT_NAME}
  VERSION ${PLUVIUS_PROJECT_VERSION}
  DESCRIPTION "The most nice rain prediction deep learning project that you will like"
  LANGUAGES CXX)

include(cmake/StandardProjectSettings.cmake)

include(packages.cmake)
add_subdirectory(src)
add_subdirectory(bin)
