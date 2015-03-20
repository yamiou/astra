#ifndef PLUGINSERVICE_H
#define PLUGINSERVICE_H

#include "SenseKit-private.h"
#include "StreamRegistry.h"
#include "StreamSetFactory.h"
#include "Stream.h"
#include "StreamBin.h"

namespace sensekit
{
    class SenseKitContext;

    using stream_handle = void*;

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
        sensekit_status_t register_stream(StreamSetId setId, StreamTypeId typeId, /*out*/stream_handle& handle);// , stream_antifactory_callback); //I created the stream, I'm letting the core fx know about it
        sensekit_status_t unregister_stream(stream_handle& handle); //stream no longer available, nominally on plugin shutdown
        sensekit_status_t orbbec_stream_create_bin(stream_handle handle, unsigned int byte_length, /*out*/ StreamBinId& id, /*out*/sensekit_frame_t*& binBuffer);
        sensekit_status_t orbbec_stream_destroy_bin(stream_handle handle, StreamBinId& id, sensekit_frame_t*& old_buf);
        //orbbec_error orbbec_stream_subscribe_client_added_event(stream_handle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_subscribe_client_removed_event(stream_handle handle, ...); //and unsubscribe...
        //orbbec_error orbbec_stream_assign_client_to_bin(stream_handle handle, client_id id, bin_id id);

        sensekit_status_t orbbec_swap_bin_buffer(stream_handle handle, StreamBinId id, /*out*/sensekit_frame_t*& new_buf);

        //sensekit_status_t orbbec_stream_swap_bin_buffers(stream_handle handle, /*bin_id id,*/ unsigned int byte_length, void** m_frontBuffer, void** backBuffer);
        //orbbec_error orbbec_stream_register_get_parameter_callback(component_handle handle, client_id client, ...);
        //orbbec_error orbbec_stream_register_set_parameter_callback(component_handle handle, client_id client, ...);

        //internal:
        sensekit_status_t get_bin(StreamBinId id, sensekit_frame_t*& buffer)
            {
                m_stream->get_bin_by_id(id);
                return SENSEKIT_STATUS_SUCCESS;
            }

    private:

        Stream* m_stream{nullptr};
        SenseKitContext& m_context;

    };
}

#endif /* PLUGINSERVICE_H */