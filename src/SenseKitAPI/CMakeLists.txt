set (_projname "SenseKitAPI")

set(${_projname}_SOURCES
  SenseKitAPI.h
  SenseKitAPI.cpp
  )

set(${_projname}_SOURCES_GEN
  SenseKitAPI.cpp.lpp
  )

add_definitions(-DSENSEKIT_BUILD_API_PROXY)
add_definitions(-DSENSEKIT_BUILD)

source_group(templates FILES ${${_projname}_SOURCES_GEN})

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_SOURCES_GEN})

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION bin/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)