#ifndef SENSEKIT_TYPES_H
#define SENSEKIT_TYPES_H

#include <stdint.h>
#include <stddef.h>

const uint8_t SENSEKIT_MAX_READER_STREAMS = 16;

const int SENSEKIT_TIMEOUT_FOREVER = -1;
const int SENSEKIT_TIMEOUT_RETURN_IMMEDIATELY = 0;

typedef struct _sensekit_stream* sensekit_stream_t;

typedef int32_t sensekit_stream_type_t;
typedef int32_t sensekit_stream_subtype_t;
typedef int32_t sensekit_frame_index_t;

const sensekit_stream_subtype_t DEFAULT_SUBTYPE = 0;

typedef struct {
    sensekit_stream_type_t type;
    sensekit_stream_subtype_t subtype;
} sensekit_stream_desc_t;

typedef struct _sensekit_streamset* sensekit_streamset_t;
typedef struct _sensekit_streamconnection_handle* sensekit_streamconnection_handle_t;

typedef struct _sensekit_streamsetconnection* sensekit_streamsetconnection_t;
typedef struct _sensekit_streamconnection* sensekit_streamconnection_t;

typedef struct _sensekit_frame sensekit_frame_t;

typedef struct _sensekit_reader* sensekit_reader_t;
typedef struct _sensekit_reader_frame* sensekit_reader_frame_t;

typedef void* sensekit_parameter_data_t;
typedef int32_t sensekit_parameter_id;
typedef int32_t sensekit_command_id;

typedef struct _sensekit_parameter_bin* sensekit_result_token_t;
typedef struct _sensekit_parameter_bin* sensekit_parameter_bin_t;

typedef struct _sensekit_bin* sensekit_bin_t;

typedef enum {
    SENSEKIT_STATUS_SUCCESS = 0,
    SENSEKIT_STATUS_INVALID_PARAMETER = 1,
    SENSEKIT_STATUS_DEVICE_ERROR = 2,
    SENSEKIT_STATUS_TIMEOUT = 3,
    SENSEKIT_STATUS_INVALID_PARAMETER_TOKEN = 4,
    SENSEKIT_STATUS_INVALID_OPERATION = 5,
    SENSEKIT_STATUS_INTERNAL_ERROR = 6
} sensekit_status_t;

typedef size_t sensekit_callback_id_t;

typedef struct _sensekit_reader_callback_id* sensekit_reader_callback_id_t;

typedef void(*sensekit_frame_ready_callback_t)(void* clientTag,
                                               sensekit_reader_t reader,
                                               sensekit_reader_frame_t frame);

typedef enum {
    SK_FATAL,
    SK_ERROR,
    SK_WARN,
    SK_INFO,
    SK_DEBUG,
    SK_TRACE
} sensekit_log_severity_t;

typedef uint32_t sensekit_event_id;

#endif /* SENSEKIT_TYPES_H */
