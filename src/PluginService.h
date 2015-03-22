#ifndef PLUGINSERVICE_H
#define PLUGINSERVICE_H

#include "SenseKit-private.h"
#include "StreamSetFactory.h"
#include "Stream.h"
#include "StreamBin.h"

namespace sensekit
{
    class SenseKitContext;
    class StreamSet;

    using StreamHandle = void*;

    class PluginService
    {
    public:
        PluginService(SenseKitContext& context)
            : m_context(context)
            {}

        StreamSet* create_stream_set();

        //metadata = int num_steam_types, StreamTypeId[] ids
        //for generators (no requirements, i.e. depth sensor and color sensor) plugin would directly create and register the streams, without using stream_factory
        //orbbec_error orbbec_register_stream_factory(StreamTypeId id, transformer_metadata md, stream_factory__callback); //callback gets passed a context_id
        //orbbec_error orbbec_unregister_stream_factory(...); //nominally on plugin shutdown
        //factory is called, plugin creates streams, then calls the stream_register to let the fx know about them
        sensekit_status_t register_stream(StreamSetId setId, StreamTypeId typeId, /*out*/StreamHandle& handle);// , stream_antifactory_callback); //I created the stream, I'm letting the core fx know about it
        sensekit_status_t unregister_stream(StreamHandle& handle); //stream no longer available, nominally on plugin shutdown
        sensekit_status_t create_stream_bin(StreamHandle handle, unsigned int byte_length, /*out*/ StreamBinId& id, /*out*/sensekit_frame_t*& binBuffer);
        sensekit_status_t destroy_stream_bin(StreamHandle handle, StreamBinId& id, sensekit_frame_t*& old_buf);
        //orbbec_error orbbec_stream_subscribe_client_added_event(StreamHandle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_subscribe_client_removed_event(StreamHandle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_assign_client_to_bin(StreamHandle handle, client_id id, bin_id id);

        sensekit_status_t cycle_bin_buffers(StreamHandle handle, StreamBinId id, /*out*/sensekit_frame_t*& binBuffer);

        //sensekit_status_t orbbec_stream_swap_bin_buffers(StreamHandle handle, /*bin_id id,*/ unsigned int byte_length, void** m_frontBuffer, void** backBuffer);
        //orbbec_error orbbec_stream_register_get_parameter_callback(component_handle handle, client_id client, ...);
        //orbbec_error orbbec_stream_register_set_parameter_callback(component_handle handle, client_id client, ...);

        Stream* get_temp_stream() { return m_stream; }

    private:
        Stream* m_stream{nullptr};
        SenseKitContext& m_context;

    };
}

#endif /* PLUGINSERVICE_H */