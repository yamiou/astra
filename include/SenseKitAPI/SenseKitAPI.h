#ifndef STREAMKITAPI_H
#define STREAMKITAPI_H

#include <SenseKit.h>
#include <Plugins/StreamServiceProxyBase.h>

SENSEKIT_BEGIN_DECLS

#ifndef SENSEKIT_API_PTR
#  ifdef SENSEKIT_BUILD_API
#    define SENSEKIT_API_PTR SENSEKIT_PUBLIC
#  else
#    define SENSEKIT_API_PTR extern
#  endif
#endif

SENSEKIT_API_PTR StreamServiceProxyBase* g_proxyPtr = nullptr;

SENSEKIT_END_DECLS

#endif /* STREAMKITAPI_H */
