/* THIS FILE AUTO-GENERATED FROM StreamServiceProxyBase.cpp.lpp. DO NOT EDIT. */
#ifndef STREAMSERVICEPROXYBASE_H
#define STREAMSERVICEPROXYBASE_H

#include <sensekit_core.h>

struct StreamServiceProxyBase
{
    void* streamService;

    sensekit_status_t (*initialize)(void*^PARAM-TYPES^);
	
    void (*terminate)(void*^PARAM-TYPES^);
	
    sensekit_status_t (*streamset_open)(void*^PARAM-TYPES^);
	
    sensekit_status_t (*streamset_close)(void*^PARAM-TYPES^);
	
    char* (*get_status_string)(void*^PARAM-TYPES^);
	
    sensekit_status_t (*reader_create)(void*^PARAM-TYPES^);
	
    sensekit_status_t (*reader_destroy)(void*^PARAM-TYPES^);
	
    sensekit_status_t (*reader_get_stream)(void*^PARAM-TYPES^);
	
};

#endif /* STREAMSERVICEPROXYBASE_H */
