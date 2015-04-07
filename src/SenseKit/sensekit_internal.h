#ifndef SENSEKIT_INTERNAL_H
#define SENSEKIT_INTERNAL_H

#include <sensekit_types.h>

struct _sensekit_streamconnection {
    sensekit_streamconnection_handle_t handle;
    sensekit_stream_desc_t desc;
};

#endif /* SENSEKIT_INTERNAL_H */
