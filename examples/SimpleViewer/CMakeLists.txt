find_package(GLUT REQUIRED)
find_package(OpenGL REQUIRED)

set (SimpleViewer_SOURCES
  main.cpp
  utils.h
  SimpleViewer.h
  SimpleViewer.cpp
  )

add_executable(SimpleViewer ${SimpleViewer_SOURCES})

include_directories(SimpleViewer ${OPENGL_INCLUDE_DIRS} ${GLUT_INCLUDE_DIR})

target_link_libraries(SimpleViewer SenseKit SenseKitUL ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})

if (WIN32)
  SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /SAFESEH:NO")

#       add_custom_command(TARGET SimpleViewer POST_BUILD
#               COMMAND ${CMAKE_COMMAND} -E copy_if_different
#                       "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
#                       $<TARGET_FILE_DIR:SimpleViewer>)
#
#       add_custom_command(TARGET SimpleViewer POST_BUILD
#               COMMAND ${CMAKE_COMMAND} -E copy_if_different
#                       "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
#                       $<TARGET_FILE_DIR:SimpleViewer>)

        add_custom_target(copyoutput ALL
          #SimpleViewer
                        COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:SimpleViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:SimpleViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:SimpleViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:SimpleViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:SimpleViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:SimpleViewer>/Plugins"
          #DepthViewer
                        COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:DepthViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:DepthViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:DepthViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:DepthViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthViewer>/Plugins"
          #DepthReaderCPP
                        COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:DepthReaderCPP>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:DepthReaderCPP>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:DepthReaderCPP>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:DepthReaderCPP>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReaderCPP>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReaderCPP>/Plugins"
          #DepthReader
                        COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:DepthReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:DepthReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:DepthReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:DepthReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReader>/Plugins"
          #SimpleColorViewer
                                COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:SimpleColorViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:SimpleColorViewer>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:SimpleColorViewer>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:SimpleColorViewer>/Plugins"
          #DepthReaderEvent
                                COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:DepthReaderEvent>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:DepthReaderEvent>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:DepthReaderEvent>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:DepthReaderEvent>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReaderEvent>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:DepthReaderEvent>/Plugins"
          #HandReader
                        COMMAND ${CMAKE_COMMAND} -E make_directory
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"
                                $<TARGET_FILE_DIR:HandReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitAPI>/SenseKitAPI.dll"
                                $<TARGET_FILE_DIR:HandReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:SenseKitUL>/SenseKitUL.dll"
                                $<TARGET_FILE_DIR:HandReader>
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:OpenNIPlugin>/OpenNIPlugin.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:orbbec_hands>/orbbec_hands.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                        COMMAND ${CMAKE_COMMAND} -E copy_if_different
                                "$<TARGET_FILE_DIR:DummyPlugin>/DummyPlugin.dll"
                                "$<TARGET_FILE_DIR:HandReader>/Plugins"
                                )
endif()
