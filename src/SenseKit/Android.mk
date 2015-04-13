LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

# Sources
MY_SRC_FILES := \
SenseKitContext.cpp \
SenseKit.cpp \
Stream.cpp \
StreamBackend.cpp \
PluginService.cpp \
StreamSet.cpp \
StreamConnection.cpp \
StreamBin.cpp \
StreamReader.cpp \
Core/shared_library_linux.cpp

LOCAL_SRC_FILES := $(MY_SRC_FILES)

# C/CPP Flags
LOCAL_CFLAGS += $(SENSEKIT_CFLAGS) -DSENSEKIT_BUILD

# Includes
LOCAL_C_INCLUDES := \
$(LOCAL_PATH)/../../include \
$(LOCAL_PATH)/../SenseKitAPI

# Output
LOCAL_SHARED_LIBRARIES := libSenseKitAPI
LOCAL_MODULE := libSenseKit

include $(BUILD_SHARED_LIBRARY)
