if (ASTRA_WINDOWS)
  set(FONT_TARGET_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>")
else()
  set(FONT_TARGET_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/")
endif()

if (ASTRA_SDK_BUILD)
  install_file(${PROJECT_SOURCE_DIR}/vendor/Inconsolata.otf bin)
endif()
add_custom_target(copy-sample-files ALL
  COMMAND ${CMAKE_COMMAND} -E copy
  "${PROJECT_SOURCE_DIR}/vendor/Inconsolata.otf"
  ${FONT_TARGET_DIR})
set_target_properties(copy-sample-files PROPERTIES FOLDER CMakeCopyTargets)

if (ASTRA_OSX)
  #SFML_ROOT defined in samples/sfml/CMakeLists.txt
  add_custom_command(TARGET copy-sample-files POST_BUILD
                     COMMAND ${CMAKE_COMMAND} -E copy_directory
                     ${SFML_ROOT}/lib/
                     ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY})
  if (ASTRA_SDK_BUILD)
    install_dir(${SFML_ROOT}/lib/ lib)
    install_dir(${SFML_ROOT} samples/thirdparty/sfml-2.3.2-min)
  endif()
endif()
