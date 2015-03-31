LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

# Sources
MY_SRC_FILES := \
$(LOCAL_PATH)/*.cpp \
$(LOCAL_PATH)/Core/*.cpp \

MY_SRC_FILE_EXPANDED := $(wildcard $(MY_SRC_FILES))
LOCAL_SRC_FILES := $(MY_SRC_FILE_EXPANDED:$(LOCAL_PATH)/%=%)

# C/CPP Flags
LOCAL_CFLAGS += $(SENSEKIT_CFLAGS) -DSENSEKIT_BUILD_EX

# Includes
LOCAL_C_INCLUDES := \
$(LOCAL_PATH)/../../include/SenseKit \
$(LOCAL_PATH)/../../include/SenseKitAPI \
$(LOCAL_PATH)/../../include/SenseKitUL \

# Output
LOCAL_SHARED_LIBRARIES := libSenseKitAPI
LOCAL_MODULE := libSenseKitUL

include $(BUILD_SHARED_LIBRARY)
