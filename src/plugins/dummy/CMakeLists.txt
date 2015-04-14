set (_projname "DummyPlugin")

set (${_projname}_SOURCES
  DummyPlugin.h
  DummyPlugin.cpp
  )

add_definitions(-DSENSEKIT_BUILD)

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(${_projname} SenseKitUL)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION bin/Plugins/
        )

install_pdb(${_projname} bin/Plugins/)
