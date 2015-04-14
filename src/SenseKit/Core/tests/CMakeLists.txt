set(SenseKit_TESTS
  signal_tests.cpp)

add_executable(SenseKitTests ${SenseKit_TESTS})

set_target_properties(SenseKitTests PROPERTIES FOLDER "tests")

target_link_libraries(SenseKitTests SenseKit)
