APP_PLATFORM := android-19

# Use ARM v7a instruction set
APP_OPTIM := debug
APP_ABI := armeabi-v7a
APP_STL := c++_static
APP_CFLAGS := --std=c++11 -fexceptions
ARCH_ARM_HAVE_ARMV7A := true

NDK_TOOLCHAIN_VERSION := clang
