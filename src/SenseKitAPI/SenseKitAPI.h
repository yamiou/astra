#ifndef SENSEKITAPI_H
#define SENSEKITAPI_H

#include <sensekit_defines.h>
#include <Plugins/StreamServiceProxyBase.h>

SENSEKIT_BEGIN_DECLS

#ifndef SENSEKIT_API_PROXY
#  ifdef SENSEKIT_BUILD_API_PROXY
#    define SENSEKIT_API_PROXY SENSEKIT_EXPORT
#  else
#    define SENSEKIT_API_PROXY SENSEKIT_IMPORT
#  endif
#endif

SENSEKIT_API_PROXY StreamServiceProxyBase* sensekit_api_get_proxy();

SENSEKIT_API_PROXY void sensekit_api_set_proxy(StreamServiceProxyBase* proxy);

SENSEKIT_END_DECLS

#endif /* SENSEKITAPI_H */
