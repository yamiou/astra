#ifndef PLUGINSERVICEDELEGATE_H
#define PLUGINSERVICEDELEGATE_H

#include <sensekit_core.h>
#include "PluginService.h"
#include "SenseKitContext.h"

namespace sensekit {

    class PluginServiceDelegate {

    public:
        static sensekit_status_t register_stream_added_callback(void* service,
                                                                StreamAddedCallback callback,
                                                                CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_added_callback(callback, *callbackId);
            }

        static sensekit_status_t register_stream_removing_callback(void* service,
                                                                  StreamRemovingCallback callback,
                                                                  CallbackId* callbackId)
            {
                return static_cast<PluginService*>(service)->register_stream_removing_callback(callback, *callbackId);
            }

        static sensekit_status_t unregister_stream_added_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_added_callback(callbackId);
            }

        static sensekit_status_t unregister_stream_removing_callback(void* service, CallbackId callbackId)
            {
                return static_cast<PluginService*>(service)->unregister_stream_removing_callback(callbackId);
            }

        static sensekit_status_t create_stream_set(void* service, StreamSetHandle** setHandle)
            {
                return static_cast<PluginService*>(service)->create_stream_set(*setHandle);
            }

        static sensekit_status_t destroy_stream_set(void* service, StreamSetHandle** setHandle)
            {
                return static_cast<PluginService*>(service)->destroy_stream_set(*setHandle);
            }

        static sensekit_status_t create_stream(void* service,
                                               StreamSetHandle* setHandle,
                                               sensekit_stream_desc_t desc,
                                               stream_callbacks_t pluginCallbacks,
                                               sensekit_stream_handle_t* handle)
            {
                return static_cast<PluginService*>(service)->create_stream(setHandle, desc, pluginCallbacks, *handle);
            }

        static sensekit_status_t destroy_stream(void* service, sensekit_stream_handle_t* handle)
            {
                return static_cast<PluginService*>(service)->destroy_stream(*handle);
            }

        static sensekit_status_t create_stream_bin(void* service,
                                                   sensekit_stream_handle_t streamHandle,
                                                   size_t lengthInBytes,
                                                   sensekit_bin_handle_t* binHandle,
                                                   sensekit_frame_t** binBuffer)
            {
                return static_cast<PluginService*>(service)->create_stream_bin(streamHandle,
                                                                               lengthInBytes,
                                                                               *binHandle,
                                                                               *binBuffer);
            }

        static sensekit_status_t destroy_stream_bin(void* service,
                                                    sensekit_stream_handle_t streamHandle,
                                                    sensekit_bin_handle_t* binHandle,
                                                    sensekit_frame_t** binBuffer)
            {
                return static_cast<PluginService*>(service)->destroy_stream_bin(streamHandle, *binHandle, *binBuffer);
            }

        static sensekit_status_t cycle_bin_buffers(void* service,
                                                   sensekit_bin_handle_t binHandle,
                                                   sensekit_frame_t** binBuffer)
            {
                return static_cast<PluginService*>(service)->cycle_bin_buffers(binHandle, *binBuffer);
            }

        static sensekit_status_t link_connection_to_bin(void* service,
                                                        sensekit_streamconnection_t* connection,
                                                        sensekit_bin_handle_t binHandle)
            {
                return static_cast<PluginService*>(service)->link_connection_to_bin(connection, binHandle);
            }
    };
}

#endif /* PLUGINSERVICEDELEGATE_H */
