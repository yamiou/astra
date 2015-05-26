#ifndef HOST_EVENTS_H
#define HOST_EVENTS_H

#include "sensekit_defines.h"
#include "sensekit_types.h"
#include <stddef.h>
#include <stdint.h>

SENSEKIT_BEGIN_DECLS

sensekit_status_t sensekit_notify_host_event(sensekit_event_id id, const void* data, size_t dataSize);
sensekit_status_t sensekit_notify_resource_available(const char* resourceURI);
sensekit_status_t sensekit_notify_resource_unavailable(const char* resourceURI);

enum {
    SENSEKIT_EVENT_RESOURCE_AVAILABLE = 1,
    SENSEKIT_EVENT_RESOURCE_UNAVAILABLE = 2
};

SENSEKIT_END_DECLS

#endif /* HOST_EVENTS_H */
