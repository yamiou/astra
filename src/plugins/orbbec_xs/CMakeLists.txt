set (_projname "orbbec_xs")

set(${_projname}_SOURCES
  XSPlugin.h
  XSPlugin.cpp
  PointProcessor.h
  PointProcessor.cpp
  PointStream.h
  StylizedDepthStream.h
  )

add_library(${_projname} SHARED ${${_projname}_SOURCES})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} AstraAPI AstraUL Shiny)

include_directories(${_projname} ${SHINY_INCLUDE})

install(TARGETS ${_projname}
  RUNTIME DESTINATION bin/Plugins/
  LIBRARY DESTINATION lib/Plugins/
  )

install_pdb(${_projname} bin/Plugins/)
