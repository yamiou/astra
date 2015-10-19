#ifndef ASTRAAPI_H
#define ASTRAAPI_H

#include <astra_core/capi/astra_defines.h>
#include <astra_core/capi/astra_streamservice_proxy.h>

ASTRA_BEGIN_DECLS

#ifndef ASTRA_API_PROXY
#  ifdef ASTRA_BUILD_API_PROXY
#    define ASTRA_API_PROXY ASTRA_EXPORT
#  else
#    define ASTRA_API_PROXY ASTRA_IMPORT
#  endif
#endif

ASTRA_API_PROXY astra_streamservice_proxy_t* astra_api_get_proxy();

ASTRA_API_PROXY void astra_api_set_proxy(astra_streamservice_proxy_t* proxy);

ASTRA_END_DECLS

#endif /* ASTRAAPI_H */
