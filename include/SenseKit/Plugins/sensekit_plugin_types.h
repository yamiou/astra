#ifndef SENSEKIT_PLUGIN_TYPES_H
#define SENSEKIT_PLUGIN_TYPES_H

#include "sensekit_types.h"

typedef int32_t bin_id_t;
typedef bin_id_t StreamBinId;

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

struct stream_callbacks_t
{
    void* context;
    set_parameter_callback_t setParameterCallback;
    get_parameter_size_callback_t getParameterSizeCallback;
    get_parameter_data_callback_t getParameterDataCallback;
    connection_added_callback_t connectionAddedCallback;
    connection_removed_callback_t connectionRemovedCallback;
};

#endif /* SENSEKIT_PLUGIN_TYPES_H */
