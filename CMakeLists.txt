cmake_minimum_required(VERSION 3.22)

set(PLUVIUS_PROJECT_NAME pluvius)
set(PLUVIUS_SOURCE_DIR src)
set(PLUVIUS_SOURCE ${PLUVIUS_SOURCE_DIR}/main.cpp)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(PLUVIUS_PROJECT_VERSION 0.1)
set(PLUVIUS_LIBRARY_NAME core)
set(PLUVIUS_BINARY_NAME pluvius)

include(cmake/UniversalAppleBuild.cmake)

project(
  ${PLUVIUS_PROJECT_NAME}
  VERSION ${PLUVIUS_PROJECT_VERSION}
  DESCRIPTION "A project for coding the book hands on cpp for machine learning"
  LANGUAGES CXX)

include(cmake/StandardProjectSettings.cmake)

#add_library(project_warnings INTERFACE)
#include(cmake/CompilerWarnings.cmake)
#set_project_warnings(project_warnings)


target_link_libraries(${PLUVIUS_BINARY_NAME} PUBLIC ${PLUVIUS_LIBRARY_NAME})
target_compile_features(${PLUVIUS_BINARY_NAME} PRIVATE cxx_std_20)

include(packages.cmake)
add_subdirectory(src)
