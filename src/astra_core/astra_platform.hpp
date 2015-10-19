#ifndef ASTRA_PLATFORM_H
#define ASTRA_PLATFORM_H

#define ASTRA_PLATFORM_WIN32 1
#define ASTRA_PLATFORM_UNIX 2
#define ASTRA_PLATFORM_DARWIN 3
#define ASTRA_PLATFORM_ANDROID 4

#if defined(_WIN32)
#include "win32/astra_platform_win32.hpp"
#elif defined(__ANDROID__) && defined(__arm__)
#include "android/astra_platform_android.hpp"
#elif defined(__linux__)
#include "unix/astra_platform_unix.hpp"
#elif defined(__APPLE__)
#include "darwin/astra_platform_darwin.hpp"
#else
#error "Unsupported platform!"
#endif

#endif /* ASTRA_PLATFORM_H */
