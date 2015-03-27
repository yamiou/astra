#ifndef SENSEKIT_PLUGIN_TYPES_H
#define SENSEKIT_PLUGIN_TYPES_H

#include "sensekit_types.h"

typedef int32_t bin_id_t;
typedef bin_id_t StreamBinId;

typedef void(*SetParameterCallback)(void*, sensekit_streamconnection_t*,
                                    sensekit_parameter_id, size_t,
                                    sensekit_parameter_data_t*);

typedef void(*GetParameterSizeCallback)(void*, sensekit_streamconnection_t*,
                                        sensekit_parameter_id, size_t*);
typedef void(*GetParameterDataCallback)(void*, sensekit_streamconnection_t*,
                                        sensekit_parameter_id, size_t,
                                        sensekit_parameter_data_t*);

typedef void(*ConnectionAddedCallback)(void*, sensekit_streamconnection_t*);
typedef void(*ConnectionRemovedCallback)(void*, sensekit_streamconnection_t*);

struct StreamPluginCallbacks
{
    void* context;
    SetParameterCallback setParameterCallback;
    GetParameterSizeCallback getParameterSizeCallback;
    GetParameterDataCallback getParameterDataCallback;
    ConnectionAddedCallback connectionAddedCallback;
    ConnectionRemovedCallback connectionRemovedCallback;
};

#endif /* SENSEKIT_PLUGIN_TYPES_H */
