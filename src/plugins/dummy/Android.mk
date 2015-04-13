LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

# Sources
LOCAL_SRC_FILES := DummyPlugin.cpp

#C/CPP Flags
LOCAL_CFLAGS += $(SENSEKIT_CFLAGS) -DSENSEKIT_BUILD

# Includes
LOCAL_C_INCLUDES := \
$(LOCAL_PATH)/../../../include/

# Output
LOCAL_MODULE := libDummyPlugin

include $(BUILD_SHARED_LIBRARY)
