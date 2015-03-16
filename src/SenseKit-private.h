#ifndef SENSEKIT_PRIVATE_H
#define SENSEKIT_PRIVATE_H

#ifdef _MSC_VER
#define SENSEKIT_API __declspec(dllexport)
#endif

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>

#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#endif
#include <math.h>
#include <limits.h>
#include <stdio.h>

#include <SenseKit.h>

SENSEKIT_BEGIN_DECLS

#undef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#undef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifndef M_SQRT2
#define M_SQRT2 1.41421356237309504880
#endif

#ifndef M_SQRT1_2
#define M_SQRT1_2 0.707106781186547524400844362104849039
#endif

#undef  ARRAY_LENGTH
#define ARRAY_LENGTH(__array) ((int) (sizeof (__array) / sizeof (__array[0])))

#undef STRINGIFY
#undef STRINGIFY_ARG
#define STRINGIFY(macro_or_string)    STRINGIFY_ARG (macro_or_string)
#define STRINGIFY_ARG(contents)       #contents

SENSEKIT_END_DECLS

#endif // SENSEKIT_PRIVATE_H
