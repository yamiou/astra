set (_projname "openni_sensor")

set(${_projname}_HEADERS
  oni_adapter_plugin.hpp
  oni_colorstream.hpp
  oni_depthstream.hpp
  oni_device_streamset.hpp
  oni_devicestream.hpp
  oni_infrared_stream.hpp
  oni_mappers.hpp
  oni_stream.hpp
  )

set(${_projname}_SOURCES
  oni_adapter_plugin.cpp
  oni_colorstream.cpp
  oni_depthstream.cpp
  oni_device_streamset.cpp
  oni_infrared_stream.cpp
  oni_mappers.cpp
  )

add_definitions(-DASTRA_BUILD)

if (WIN32)
  if(ASTRA_64)
    set(ONI_LIB_DIR $ENV{OPENNI2_LIB64})
  else()
    set(ONI_LIB_DIR $ENV{OPENNI2_LIB})
  endif()
  
  message("ONI_LIB_DIR ${ONI_LIB_DIR}")

  find_library(OpenNI2_LIBRARY NAMES "OpenNI2.lib" HINTS ${ONI_LIB_DIR} PATH_SUFFIXES lib)
elseif (APPLE)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.dylib" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
elseif (UNIX AND NOT ANDROID)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.so" HINTS "$ENV{OPENNI2_REDIST}" PATH_SUFFIXES lib)
elseif (ANDROID)
  find_library(OpenNI2_LIBRARY NAMES "libOpenNI2.so" PATHS "${CMAKE_SOURCE_DIR}/vendor/openni-android" NO_CMAKE_FIND_ROOT_PATH)
endif()

include_directories ("$ENV{OPENNI2_INCLUDE}")

include_directories(${_projname} ${SHINY_INCLUDE})

add_library(${_projname} SHARED ${${_projname}_SOURCES} ${${_projname}_HEADERS})

set_target_properties(${_projname} PROPERTIES FOLDER "plugins")

target_link_libraries(${_projname} AstraAPI Shiny ${OpenNI2_LIBRARY})

install_lib(${_projname} "Plugins/")
