set (_projname "dummy_plugin")

set (${_projname}_SOURCES
  DummyPlugin.h
  DummyPlugin.cpp
  )

add_definitions(-DSENSEKIT_BUILD)

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} SenseKitUL)

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION lib/Plugins/
        )

install_pdb(${_projname} bin/Plugins/)
