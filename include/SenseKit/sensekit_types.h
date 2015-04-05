#ifndef SENSEKIT_TYPES_H
#define SENSEKIT_TYPES_H

#include <stdint.h>
#include <stddef.h>

const unsigned MAX_STRING_FIELD_LENGTH = 256;
const uint8_t SENSEKIT_MAX_READER_STREAMS = 16;

typedef struct _sensekit_stream_handle* sensekit_stream_handle_t;
typedef int32_t sensekit_stream_type_t;
typedef int32_t sensekit_stream_subtype_t;

const sensekit_stream_subtype_t DEFAULT_SUBTYPE = -1;

typedef sensekit_stream_type_t StreamType;
typedef sensekit_stream_subtype_t StreamSubtype;

struct sensekit_stream_desc_t {
    sensekit_stream_type_t type;
    sensekit_stream_subtype_t subType;
};

typedef struct _sensekit_streamset sensekit_streamset_t;
typedef struct _sensekit_streamconnection_handle sensekit_streamconnection_handle;

typedef struct _sensekit_streamconnection {
    sensekit_streamconnection_handle* handle;
    sensekit_stream_desc_t desc;
} sensekit_streamconnection_t;

typedef struct _sensekit_frame {
    size_t byteLength;
    void* data;
} sensekit_frame_t;

typedef struct _sensekit_frame_ref {
    sensekit_streamconnection_t* streamConnection;
    sensekit_frame_t* frame;
} sensekit_frame_ref_t;

typedef struct _sensekit_reader sensekit_reader_t;
typedef sensekit_reader_t sensekit_reader_frame_t;

typedef struct _sensekit_parameter_data sensekit_parameter_data_t;
typedef int32_t sensekit_parameter_id;

typedef sensekit_stream_handle_t StreamHandle;

typedef sensekit_streamset_t StreamSetHandle;

typedef struct _sensekit_bin_handle* sensekit_bin_handle_t;

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

typedef int32_t bin_id_t;
typedef bin_id_t StreamBinId;

// Stream callback types
typedef void(*set_parameter_callback_t)(void*, sensekit_streamconnection_t*,
                                        sensekit_parameter_id, size_t,
                                        sensekit_parameter_data_t*);

typedef void(*get_parameter_size_callback_t)(void*, sensekit_streamconnection_t*,
                                             sensekit_parameter_id, size_t*);

typedef void(*get_parameter_data_callback_t)(void*, sensekit_streamconnection_t*,
                                             sensekit_parameter_id, size_t,
                                             sensekit_parameter_data_t*);

typedef void(*connection_added_callback_t)(void*, sensekit_streamconnection_t*);
typedef void(*connection_removed_callback_t)(void*, sensekit_streamconnection_t*);


typedef void(*StreamAddedCallback)(StreamSetHandle*,
                                   sensekit_stream_handle_t,
                                   sensekit_stream_desc_t);

typedef void(*StreamRemovingCallback)(StreamSetHandle*,
                                      sensekit_stream_handle_t,
                                      sensekit_stream_desc_t);
typedef size_t CallbackId;

struct stream_callbacks_t
{
    void* context;
    set_parameter_callback_t setParameterCallback;
    get_parameter_size_callback_t getParameterSizeCallback;
    get_parameter_data_callback_t getParameterDataCallback;
    connection_added_callback_t connectionAddedCallback;
    connection_removed_callback_t connectionRemovedCallback;
};
#endif /* SENSEKIT_TYPES_H */
