# LOCAL_PATH := $(call my-dir)

# include $(CLEAR_VARS)

# LOCAL_MODULE := libOpenCV
# LOCAL_SRC_FILES := \
#         ../../../lib/libopencv_core.so \
#         ../../../lib/libopencv_imgproc.so \

# LOCAL_EXPORT_C_INCLUDES := /usr/local/include/opencv

# include $(PREBUILT_SHARED_LIBRARY)

# include $(CLEAR_VARS)

# # Sources
# MY_SRC_FILES := \
# $(LOCAL_PATH)/*.cpp

# MY_SRC_FILE_EXPANDED := $(wildcard $(MY_SRC_FILES))
# LOCAL_SRC_FILES := $(MY_SRC_FILE_EXPANDED:$(LOCAL_PATH)/%=%)

# #C/CPP Flags
# LOCAL_CFLAGS += $(SENSEKIT_CFLAGS) -DSENSEKIT_BUILD

# # Includes
# LOCAL_C_INCLUDES := \
# $(LOCAL_PATH)/../../../include/

# # Output
# LOCAL_SHARED_LIBRARIES := libOpenCV libSenseKitAPI
# LOCAL_MODULE := libOrbbecHand

# include $(BUILD_SHARED_LIBRARY)
