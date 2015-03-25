#ifndef SENSEKIT_PLUGIN_TYPES_H
#define SENSEKIT_PLUGIN_TYPES_H

#include "sensekit_types.h"

typedef int32_t bin_id_t;

using SetParameterCallback = void(*)(void*, sensekit_streamconnection_t*, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
using GetParameterSizeCallback = void(*)(void*, sensekit_streamconnection_t*, sensekit_parameter_id, /*out*/size_t*);
using GetParameterDataCallback = void(*)(void*, sensekit_streamconnection_t*, sensekit_parameter_id, size_t, sensekit_parameter_data_t*);
using ConnectionAddedCallback = void(*)(void*, sensekit_streamconnection_t*);
using ConnectionRemovedCallback = void(*)(void*, sensekit_streamconnection_t*);

struct StreamPluginCallbacks
{
    void* context;
    SetParameterCallback setParameterCallback;
    GetParameterSizeCallback getParameterSizeCallback;
    GetParameterDataCallback getParameterDataCallback;
    ConnectionAddedCallback connectionAddedCallback;
    ConnectionRemovedCallback connectionRemovedCallback;

    StreamPluginCallbacks(void* context) :
        context(context),
        setParameterCallback(nullptr),
        getParameterSizeCallback(nullptr),
        getParameterDataCallback(nullptr),
        connectionAddedCallback(nullptr),
        connectionRemovedCallback(nullptr)
    {
    }
};


#endif /* SENSEKIT_PLUGIN_TYPES_H */
