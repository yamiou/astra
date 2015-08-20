set (_projname "AstraAPI")

set(${_projname}_SOURCES
  AstraAPI.h
  AstraAPI.cpp
  )

set(${_projname}_SOURCES_GEN
  AstraAPI.cpp.lpp
  )

add_definitions(-DASTRA_BUILD_API_PROXY)
add_definitions(-DASTRA_BUILD)

source_group(templates FILES ${${_projname}_SOURCES_GEN})

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_SOURCES_GEN})

install(TARGETS ${_projname}
        RUNTIME DESTINATION bin/
        LIBRARY DESTINATION lib/
        ARCHIVE DESTINATION lib/
        )

install_pdb(${_projname} bin/)
