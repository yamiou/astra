LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := libOpenNI2
LOCAL_SRC_FILES := ../../../lib/libOpenNI2.so
LOCAL_EXPORT_C_INCLUDES := /usr/local/include/ni2

include $(PREBUILT_SHARED_LIBRARY)

include $(CLEAR_VARS)

# Sources
LOCAL_SRC_FILES := OpenNIPlugin.cpp

#C/CPP Flags
LOCAL_CFLAGS += $(SENSEKIT_CFLAGS) -DSENSEKIT_BUILD

# Includes
LOCAL_C_INCLUDES := \
$(LOCAL_PATH)/../../../include/SenseKit \
$(LOCAL_PATH)/../../../include/SenseKitUL \

# Output
LOCAL_SHARED_LIBRARIES := libOpenNI2 libSenseKitAPI
LOCAL_MODULE := libOpenNIPlugin

include $(BUILD_SHARED_LIBRARY)
