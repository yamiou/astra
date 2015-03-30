#ifndef SENSEKIT_CORE_H
#define SENSEKIT_CORE_H

#ifdef  __cplusplus
# define SENSEKIT_BEGIN_DECLS  extern "C" {
# define SENSEKIT_END_DECLS    }
#else
# define SENSEKIT_BEGIN_DECLS
# define SENSEKIT_END_DECLS
#endif

# if defined (_MSC_VER)
#  define SENSEKIT_EXPORT __declspec(dllexport)
#  define SENSEKIT_IMPORT __declspec(dllimport)
#  define SENSEKIT_PUBLIC
#  define SENSEKIT_LOCAL
# else
#  if __GNUC__ >= 4
#    define SENSEKIT_PUBLIC __attribute__ ((visibility ("default")))
#    define SENSEKIT_LOCAL  __attribute__ ((visibility ("hidden")))
#    define SENSEKIT_EXPORT SENSEKIT_PUBLIC
#    define SENSEKIT_IMPORT
#  else
#    define SENSEKIT_PUBLIC
#    define SENSEKIT_LOCAL
#    define SENSEKIT_EXPORT
#    define SENSEKIT_IMPORT
#  endif
# endif

#ifndef SENSEKIT_API
#  ifdef SENSEKIT_BUILD
#    define SENSEKIT_API SENSEKIT_EXPORT
#  else
#    define SENSEKIT_API SENSEKIT_IMPORT
#  endif
#endif

#ifndef SENSEKIT_API_EX
#  ifdef SENSEKIT_BUILD_EX
#    define SENSEKIT_API_EX SENSEKIT_EXPORT
#  else
#    define SENSEKIT_API_EX SENSEKIT_IMPORT
#  endif
#endif

#endif /* SENSEKIT_CORE_H */
