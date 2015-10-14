if (ASTRA_SDK_BUILD)
  if (ASTRA_WINDOWS)
    set(FONT_TARGET_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>")
  else()
    set(FONT_TARGET_DIR "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
  endif()

  install_file(${PROJECT_SOURCE_DIR}/vendor/Inconsolata.otf bin)
  add_custom_target(copyfont ALL
    COMMAND ${CMAKE_COMMAND} -E copy
    "${PROJECT_SOURCE_DIR}/vendor/Inconsolata.otf"
    ${FONT_TARGET_DIR})
  set_target_properties(copyfont PROPERTIES FOLDER CMakeCopyTargets)
endif()
