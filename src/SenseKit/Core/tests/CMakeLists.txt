set(SenseKit_TESTS
  signal_tests.cpp)

add_executable(SenseKitTests ${SenseKit_TESTS})

target_link_libraries(SenseKitTests SenseKit)
