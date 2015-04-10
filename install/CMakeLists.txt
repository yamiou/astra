set(sdk_INCLUDES
  ../../include/SenseKit/SenseKit.h
  ../../include/SenseKit/sensekit_capi.h
  ../../include/SenseKit/sensekit_defines.h
  ../../include/SenseKit/sensekit_types.h
  ../../include/SenseKitUL/SenseKitUL.h
  ../../include/SenseKitUL/skul_capi.h
  ../../include/SenseKitUL/StreamTypes.h
  ../../include/SenseKitUL/streams/depth.h
  ../../include/SenseKitUL/streams/depth_capi.h
  ../../include/SenseKitUL/streams/depth_types.h
  ../../include/SenseKitUL/streams/color.h
  ../../include/SenseKitUL/streams/color_capi.h
  ../../include/SenseKitUL/streams/color_types.h
  )

add_custom_target(SDK ${sdk_INCLUDES})