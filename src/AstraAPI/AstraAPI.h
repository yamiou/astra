#ifndef ASTRAAPI_H
#define ASTRAAPI_H

#include <Astra/astra_defines.h>
#include <Astra/Plugins/StreamServiceProxyBase.h>

ASTRA_BEGIN_DECLS

#ifndef ASTRA_API_PROXY
#  ifdef ASTRA_BUILD_API_PROXY
#    define ASTRA_API_PROXY ASTRA_EXPORT
#  else
#    define ASTRA_API_PROXY ASTRA_IMPORT
#  endif
#endif

ASTRA_API_PROXY StreamServiceProxyBase* astra_api_get_proxy();

ASTRA_API_PROXY void astra_api_set_proxy(StreamServiceProxyBase* proxy);

ASTRA_END_DECLS

#endif /* ASTRAAPI_H */
