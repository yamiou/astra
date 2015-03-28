#ifndef SENSEKIT_CORE_H
#define SENSEKIT_CORE_H

#ifdef  __cplusplus
# define SENSEKIT_BEGIN_DECLS  extern "C" {
# define SENSEKIT_END_DECLS    }
# include <cstdint>
# include <cstddef>
#else
# define SENSEKIT_BEGIN_DECLS
# define SENSEKIT_END_DECLS
# include <stdint.h>
# include <stddef.h>
#endif

# if defined (_MSC_VER)
#  define SENSEKIT_EXPORT __declspec(dllexport)
#  define SENSEKIT_IMPORT __declspec(dllimport)
# else
#  if __GNUC__ >= 4
#    define SENSEKIT_EXPORT __attribute__ ((visibility ("default")))
#    define SENSEKIT_IMPORT
#  else
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

#endif /* SENSEKIT_CORE_H */
