#ifndef SENSEKIT_TYPES_H
#define SENSEKIT_TYPES_H

#include "sensekit_core.h"

const unsigned MAX_STRING_FIELD_LENGTH = 256;

typedef struct _sensekit_streamset sensekit_streamset_t;
typedef struct _sensekit_streamconnection sensekit_streamconnection_t;

typedef struct _sensekit_parameter_data sensekit_parameter_data_t;
typedef int32_t sensekit_parameter_id;

typedef struct _sensekit_stream sensekit_stream_t;
typedef unsigned sensekit_stream_type_t;
typedef unsigned sensekit_stream_subtype_t;

typedef sensekit_stream_type_t StreamType;
typedef sensekit_stream_subtype_t StreamSubtype;
typedef sensekit_stream_t StreamHandle;

typedef sensekit_streamset_t StreamSetHandle;

typedef struct _sensekit_context sensekit_context_t;

typedef enum _sensekit_status {
    SENSEKIT_STATUS_SUCCESS = 0,
    SENSEKIT_STATUS_INVALID_PARAMETER = 1,
    SENSEKIT_STATUS_DEVICE_ERROR = 2,
    SENSEKIT_STATUS_TIMEOUT = 3
} sensekit_status_t;

typedef struct _sensekit_device_desc {
    char uri[MAX_STRING_FIELD_LENGTH];
    char vendor[MAX_STRING_FIELD_LENGTH];
    char name[MAX_STRING_FIELD_LENGTH];
    uint16_t usbVendorId;
    uint16_t usbProductId;
} sensekit_device_desc_t;

typedef struct _sensekit_frame {
    size_t byteLength;
    void* data;
} sensekit_frame_t;

typedef struct _sensekit_frame_ref {
    sensekit_streamconnection_t* streamConnection;
    sensekit_frame_t* frame;
} sensekit_frame_ref_t;
#endif /* SENSEKIT_TYPES_H */
