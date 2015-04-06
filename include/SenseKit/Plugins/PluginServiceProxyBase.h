/* THIS FILE AUTO-GENERATED FROM PluginServiceProxyBase.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEPROXYBASE_H
#define PLUGINSERVICEPROXYBASE_H

#include <sensekit_core.h>

struct PluginServiceProxyBase
{
    void* pluginService;

    sensekit_status_t (*register_stream_added_callback)(void*,
                                                        StreamAddedCallback,
                                                        CallbackId*);

    sensekit_status_t (*register_stream_removing_callback)(void*,
                                                           StreamRemovingCallback,
                                                           CallbackId*);

    sensekit_status_t (*unregister_stream_added_callback)(void*,
                                                          CallbackId);

    sensekit_status_t (*unregister_stream_removing_callback)(void*,
                                                             CallbackId);

    sensekit_status_t (*create_stream_set)(void*,
                                           sensekit_streamset_t&);

    sensekit_status_t (*destroy_stream_set)(void*,
                                            sensekit_streamset_t&);

    sensekit_status_t (*create_stream)(void*,
                                       sensekit_streamset_t,
                                       sensekit_stream_desc_t,
                                       stream_callbacks_t,
                                       sensekit_stream_t*);

    sensekit_status_t (*destroy_stream)(void*,
                                        sensekit_stream_t&);

    sensekit_status_t (*create_stream_bin)(void*,
                                           sensekit_stream_t,
                                           size_t,
                                           sensekit_bin_t*,
                                           sensekit_frame_t**);

    sensekit_status_t (*destroy_stream_bin)(void*,
                                            sensekit_stream_t,
                                            sensekit_bin_t*,
                                            sensekit_frame_t**);

    sensekit_status_t (*cycle_bin_buffers)(void*,
                                           sensekit_bin_t,
                                           sensekit_frame_t**);

    sensekit_status_t (*link_connection_to_bin)(void*,
                                                sensekit_streamconnection_t*,
                                                sensekit_bin_t);

};

#endif /* PLUGINSERVICEPROXYBASE_H */
