file(GLOB HEADER_LIST
  CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/include/**/*.hpp")
file(GLOB SOURCE_LIST
  "${PROJECT_SOURCE_DIR}/src/*.cpp"
  "${PROJECT_SOURCE_DIR}/src/**/*.cpp"
)

add_library(${PLUVIUS_LIBRARY_NAME} ${SOURCE_LIST} ${HEADER_LIST})


target_link_libraries (${PLUVIUS_LIBRARY_NAME} dlib::dlib)
target_include_directories(${PLUVIUS_LIBRARY_NAME} PUBLIC ../include)

target_compile_features(${PLUVIUS_LIBRARY_NAME} PRIVATE cxx_std_20)

add_subdirectory(feature_extractor)
