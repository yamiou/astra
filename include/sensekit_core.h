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

#ifndef SENSEKIT_API
# if defined (_MSC_VER) && ! defined (SENSEKIT_WIN32_STATIC_BUILD)
#  define SENSEKIT_API __declspec(dllimport)
# else
#  define SENSEKIT_API
# endif
#endif

#endif /* SENSEKIT_CORE_H */
