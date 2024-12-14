set(FEATURE_EXTRACTOR_BIN feature-extractor)

add_executable(${FEATURE_EXTRACTOR_BIN} feature_extractor.cpp)
target_link_libraries(${FEATURE_EXTRACTOR_BIN} PUBLIC ${PLUVIUS_LIBRARY_NAME})
target_compile_features(${FEATURE_EXTRACTOR_BIN} PRIVATE cxx_std_20)

