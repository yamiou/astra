#ifndef PLUGIN_CAPI_H
#define PLUGIN_CAPI_H

#include <SenseKit/sensekit_capi.h>
#include <SenseKit/Plugins/plugin_callbacks.h>
#include <SenseKit/Plugins/StreamServiceProxyBase.h>
#include <SenseKit/Plugins/PluginServiceProxyBase.h>

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

#endif /* PLUGIN_CAPI_H */
