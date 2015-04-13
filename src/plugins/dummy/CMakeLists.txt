set (DummyPlugin_SOURCES
  DummyPlugin.h
  DummyPlugin.cpp
  )

add_definitions(-DSENSEKIT_BUILD)

add_library(DummyPlugin SHARED ${DummyPlugin_SOURCES})

# force c++11 (has no effect currently)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD 11)
#set_property(TARGET SenseKit PROPERTY CXX_STANDARD_REQUIRED ON)

target_link_libraries(DummyPlugin SenseKitUL)

install(TARGETS DummyPlugin
        RUNTIME DESTINATION bin/Plugins/
        LIBRARY DESTINATION bin/Plugins/
        )

install_pdb(DummyPlugin bin/Plugins/)
