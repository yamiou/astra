make_directory("${CMAKE_CURRENT_LIST_DIR}/src/main/jniLibs/armeabi-v7a")

add_custom_target(SenseKitAndroid ALL
  COMMENT "Executing gradle build"
  COMMAND ${CMAKE_COMMAND}
  -D"INPATH=${CMAKE_SOURCE_DIR}/vendor/openni-android/"
  -D"OUTPATH=${CMAKE_CURRENT_LIST_DIR}/src/main/jniLibs/armeabi-v7a/"
  -D"PATTERN=*.so"
  -P "CopyFiles.cmake"
  COMMAND ${CMAKE_COMMAND}
  -D"INPATH=${CMAKE_BINARY_DIR}/lib/"
  -D"OUTPATH=${CMAKE_CURRENT_LIST_DIR}/src/main/jniLibs/armeabi-v7a/"
  -D"PATTERN=*.so"
  -P "CopyFiles.cmake"
  COMMAND ${CMAKE_COMMAND}
  -D"INPATH=${CMAKE_BINARY_DIR}/bin/Plugins/"
  -D"OUTPATH=${CMAKE_CURRENT_LIST_DIR}/src/main/jniLibs/armeabi-v7a/"
  -D"PATTERN=*.so"
  -P "CopyFiles.cmake"
  COMMAND "${CMAKE_CURRENT_LIST_DIR}/gradlew" "build"
  WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}")

add_dependencies(SenseKitAndroid sensekit_jni ${SDK_DEPENDENT_TARGET})

install(FILES ${CMAKE_CURRENT_LIST_DIR}/build/outputs/aar/sensekit-android-release.aar
  DESTINATION aar/)
