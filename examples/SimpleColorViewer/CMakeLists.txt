find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)

set (SimpleColorViewer_SOURCES
  main.cpp
  utils.h
  SimpleViewer.h
  SimpleViewer.cpp
  )

add_executable(SimpleColorViewer ${SimpleColorViewer_SOURCES})

include_directories(SimpleColorViewer ${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIR})

target_link_libraries(SimpleColorViewer SenseKit SenseKitUL ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})

if (WIN32)
  SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /SAFESEH:NO")

        add_custom_target(copyoutput ALL
                #SimpleColorViewer
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitSL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                                )
endif()
