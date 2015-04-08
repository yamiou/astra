/* THIS FILE AUTO-GENERATED FROM PluginServiceProxyBase.h.lpp. DO NOT EDIT. */
#ifndef PLUGINSERVICEPROXYBASE_H
#define PLUGINSERVICEPROXYBASE_H

#include <sensekit_core.h>

struct PluginServiceProxyBase
{
    void* pluginService;

    sensekit_status_t (*register_stream_added_callback)(void*,
                                                        stream_added_callback_t,
                                                        sensekit_callback_id_t*);

    sensekit_status_t (*register_stream_removing_callback)(void*,
                                                           stream_removing_callback_t,
                                                           sensekit_callback_id_t*);

    sensekit_status_t (*unregister_stream_added_callback)(void*,
                                                          sensekit_callback_id_t);

    sensekit_status_t (*unregister_stream_removing_callback)(void*,
                                                             sensekit_callback_id_t);

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

    sensekit_status_t (*bin_has_connections)(void*,
                                             sensekit_bin_t,
                                             bool*);

    sensekit_status_t (*cycle_bin_buffers)(void*,
                                           sensekit_bin_t,
                                           sensekit_frame_t**);

    sensekit_status_t (*link_connection_to_bin)(void*,
                                                sensekit_streamconnection_t,
                                                sensekit_bin_t);

};

#endif /* PLUGINSERVICEPROXYBASE_H */
