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

target_link_libraries(SimpleViewer SenseKit SenseKitSL ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})

if (WIN32)
  SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /SAFESEH:NO")
  SET (CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} /SAFESEH:NO")

#	add_custom_command(TARGET SimpleViewer POST_BUILD       
#		COMMAND ${CMAKE_COMMAND} -E copy_if_different 
#			"$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"      
#			$<TARGET_FILE_DIR:SimpleViewer>)                
#
#	add_custom_command(TARGET SimpleViewer POST_BUILD       
#		COMMAND ${CMAKE_COMMAND} -E copy_if_different  
#			"$<TARGET_FILE_DIR:SenseKitSL>/SenseKitSL.dll"  
#			$<TARGET_FILE_DIR:SimpleViewer>)         

	add_custom_target(copyoutput ALL 
			COMMAND ${CMAKE_COMMAND} -E copy_if_different 
				"$<TARGET_FILE_DIR:SenseKit>/SenseKit.dll"      
				$<TARGET_FILE_DIR:SimpleViewer>
			COMMAND ${CMAKE_COMMAND} -E copy_if_different  
				"$<TARGET_FILE_DIR:SenseKitSL>/SenseKitSL.dll"  
				$<TARGET_FILE_DIR:SimpleViewer>)
endif()
