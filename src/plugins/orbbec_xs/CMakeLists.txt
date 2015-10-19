set(XS_SRC
  xs_plugin.cpp
  xs_point_processor.cpp
  )

set(XS_INCLUDE
  xs_plugin.hpp
  xs_point_processor.hpp
  xs_pointstream.hpp
  )

include_directories(orbbec-xs-plugin ${SHINY_INCLUDE})
add_library(orbbec-xs-plugin SHARED ${XS_SRC} ${XS_INCLUDE})
target_link_libraries(orbbec-xs-plugin astra_core_api astra Shiny)

set_target_properties(orbbec-xs-plugin PROPERTIES FOLDER "plugins")
set_target_properties(orbbec-xs-plugin PROPERTIES OUTPUT_NAME "orbbec_xs")
install_lib(orbbec-xs-plugin "Plugins")
