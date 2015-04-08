#ifndef PLUGIN_API_H
#define PLUGIN_API_H

#include "../sensekit_types.h"

struct _sensekit_streamconnection {
    sensekit_streamconnection_handle_t handle;
    sensekit_stream_desc_t desc;
};

struct _sensekit_reader_callback_id {
    sensekit_reader_t reader;
    sensekit_callback_id_t callbackId;
};


struct _sensekit_frame {
    size_t byteLength;
    sensekit_frame_index_t frameIndex;
    void* data;
};

struct _sensekit_frame_ref {
    sensekit_streamconnection_t streamConnection;
    sensekit_frame_t* frame;
};

typedef void(*set_parameter_callback_t)(void*, sensekit_streamconnection_t,
                                        sensekit_parameter_id,
                                        size_t,
                                        sensekit_parameter_data_t*);

typedef void(*get_parameter_size_callback_t)(void*,
                                             sensekit_streamconnection_t,
                                             sensekit_parameter_id,
                                             size_t*);

typedef void(*get_parameter_data_callback_t)(void*,
                                             sensekit_streamconnection_t,
                                             sensekit_parameter_id, size_t,
                                             sensekit_parameter_data_t*);

typedef void(*connection_added_callback_t)(void*, sensekit_streamconnection_t);
typedef void(*connection_removed_callback_t)(void*, sensekit_streamconnection_t);


typedef void(*stream_added_callback_t)(sensekit_streamset_t,
                                       sensekit_stream_t,
                                       sensekit_stream_desc_t);

typedef void(*stream_removing_callback_t)(sensekit_streamset_t,
                                          sensekit_stream_t,
                                          sensekit_stream_desc_t);

struct stream_callbacks_t
{
    void* context;
    set_parameter_callback_t setParameterCallback;
    get_parameter_size_callback_t getParameterSizeCallback;
    get_parameter_data_callback_t getParameterDataCallback;
    connection_added_callback_t connectionAddedCallback;
    connection_removed_callback_t connectionRemovedCallback;
};

#include "PluginBase.h"

#endif /* PLUGIN_API_H */
