#ifndef SENSEKIT_INTERNAL_H
#define SENSEKIT_INTERNAL_H

#include <sensekit_types.h>

struct _sensekit_streamconnection {
    sensekit_streamconnection_handle_t handle;
    sensekit_stream_desc_t desc;
};

typedef struct _sensekit_reader_callback_id {
    sensekit_reader_t reader;
    sensekit_callback_id_t callbackId;
} *sensekit_reader_callback_id_t;

#endif /* SENSEKIT_INTERNAL_H */
