#ifndef PLUGIN_CAPI_H
#define PLUGIN_CAPI_H

#include <SenseKit/sensekit_capi.h>
#include <SenseKit/Plugins/plugin_callbacks.h>
#include <SenseKit/Plugins/StreamServiceProxyBase.h>
#include <SenseKit/Plugins/PluginServiceProxyBase.h>

#if defined(__GNUC__) || defined(__clang__)
#define PACK_STRUCT __attribute__((packed))
#else
#define PACK_STRUCT
#endif

struct _sensekit_streamconnection {
    sensekit_streamconnection_handle_t handle;
    sensekit_stream_desc_t desc;
};

struct _sensekit_reader_callback_id {
    sensekit_reader_t reader;
    sensekit_callback_id_t callbackId;
};


#ifdef _MSC_VER
#pragma pack(push, 1)
#endif

struct _sensekit_frame {
    size_t byteLength;
    sensekit_frame_index_t frameIndex;
    union {
        void* data;
        uint64_t pad0;
    };
} PACK_STRUCT;

#ifdef _MSC_VER
#pragma pack(pop)
#endif

#endif /* PLUGIN_CAPI_H */
