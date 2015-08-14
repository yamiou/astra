set (_projname "AstraTests")

set(${_projname}_TESTS
  signal_tests.cpp)

add_executable(${_projname} ${${_projname}_TESTS})

set_target_properties(${_projname} PROPERTIES FOLDER "tests")

target_link_libraries(${_projname} ${ASTRA_LIBRARIES})

set_debug_working_dir(${_projname} "$(TargetDir)")

add_astra_sdk_dependency(${_projname})

