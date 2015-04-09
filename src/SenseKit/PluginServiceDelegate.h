/* THIS FILE AUTO-GENERATED FROM PluginServiceDelegate.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEDELEGATE_H
#define PLUGINSERVICEDELEGATE_H

#include <sensekit_types.h>
#include "SenseKitContext.h"

namespace sensekit {

    class PluginServiceDelegate
    {
    public:

        static sensekit_status_t register_stream_added_callback(void* pluginService,
                                                                stream_added_callback_t callback,
                                                                void* clientTag,
                                                                sensekit_callback_id_t* callbackId)
        {
            return static_cast<PluginService*>(pluginService)->register_stream_added_callback(callback, clientTag, *callbackId);
        }

        static sensekit_status_t register_stream_removing_callback(void* pluginService,
                                                                   stream_removing_callback_t callback,
                                                                   void* clientTag,
                                                                   sensekit_callback_id_t* callbackId)
        {
            return static_cast<PluginService*>(pluginService)->register_stream_removing_callback(callback, clientTag, *callbackId);
        }

        static sensekit_status_t unregister_stream_added_callback(void* pluginService,
                                                                  sensekit_callback_id_t callback)
        {
            return static_cast<PluginService*>(pluginService)->unregister_stream_added_callback(callback);
        }

        static sensekit_status_t unregister_stream_removing_callback(void* pluginService,
                                                                     sensekit_callback_id_t callback)
        {
            return static_cast<PluginService*>(pluginService)->unregister_stream_removing_callback(callback);
        }

        static sensekit_status_t create_stream_set(void* pluginService,
                                                   sensekit_streamset_t& setHandle)
        {
            return static_cast<PluginService*>(pluginService)->create_stream_set(setHandle);
        }

        static sensekit_status_t destroy_stream_set(void* pluginService,
                                                    sensekit_streamset_t& setHandle)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream_set(setHandle);
        }

        static sensekit_status_t create_stream(void* pluginService,
                                               sensekit_streamset_t setHandle,
                                               sensekit_stream_desc_t desc,
                                               stream_callbacks_t pluginCallbacks,
                                               sensekit_stream_t* handle)
        {
            return static_cast<PluginService*>(pluginService)->create_stream(setHandle, desc, pluginCallbacks, *handle);
        }

        static sensekit_status_t destroy_stream(void* pluginService,
                                                sensekit_stream_t& handle)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream(handle);
        }

        static sensekit_status_t create_stream_bin(void* pluginService,
                                                   sensekit_stream_t streamHandle,
                                                   size_t lengthInBytes,
                                                   sensekit_bin_t* binHandle,
                                                   sensekit_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->create_stream_bin(streamHandle, lengthInBytes, *binHandle, *binBuffer);
        }

        static sensekit_status_t destroy_stream_bin(void* pluginService,
                                                    sensekit_stream_t streamHandle,
                                                    sensekit_bin_t* binHandle,
                                                    sensekit_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->destroy_stream_bin(streamHandle, *binHandle, *binBuffer);
        }

        static sensekit_status_t bin_has_connections(void* pluginService,
                                                     sensekit_bin_t binHandle,
                                                     bool* hasConnections)
        {
            return static_cast<PluginService*>(pluginService)->bin_has_connections(binHandle, *hasConnections);
        }

        static sensekit_status_t cycle_bin_buffers(void* pluginService,
                                                   sensekit_bin_t binHandle,
                                                   sensekit_frame_t** binBuffer)
        {
            return static_cast<PluginService*>(pluginService)->cycle_bin_buffers(binHandle, *binBuffer);
        }

        static sensekit_status_t link_connection_to_bin(void* pluginService,
                                                        sensekit_streamconnection_t connection,
                                                        sensekit_bin_t binHandle)
        {
            return static_cast<PluginService*>(pluginService)->link_connection_to_bin(connection, binHandle);
        }
    };
}

#endif /* PLUGINSERVICEDELEGATE_H */
