/* THIS FILE AUTO-GENERATED FROM astra_create_plugin_proxy.hpp.lpp. DO NOT EDIT. */
#ifndef ASTRA_CREATE_PLUGIN_PROXY_H
#define ASTRA_CREATE_PLUGIN_PROXY_H

#include "astra_plugin_service.hpp"
#include "astra_plugin_service_delegate.hpp"
#include <Astra/Plugins/PluginServiceProxyBase.h>

namespace astra {
PluginServiceProxyBase* create_plugin_proxy(plugin_service* service)
    {
        PluginServiceProxyBase* proxy = new PluginServiceProxyBase;

        proxy->register_stream_registered_callback = &plugin_service_delegate::register_stream_registered_callback;
        proxy->register_stream_unregistering_callback = &plugin_service_delegate::register_stream_unregistering_callback;
        proxy->register_host_event_callback = &plugin_service_delegate::register_host_event_callback;
        proxy->unregister_host_event_callback = &plugin_service_delegate::unregister_host_event_callback;
        proxy->unregister_stream_registered_callback = &plugin_service_delegate::unregister_stream_registered_callback;
        proxy->unregister_stream_unregistering_callback = &plugin_service_delegate::unregister_stream_unregistering_callback;
        proxy->create_stream_set = &plugin_service_delegate::create_stream_set;
        proxy->destroy_stream_set = &plugin_service_delegate::destroy_stream_set;
        proxy->get_streamset_uri = &plugin_service_delegate::get_streamset_uri;
        proxy->create_stream = &plugin_service_delegate::create_stream;
        proxy->register_stream = &plugin_service_delegate::register_stream;
        proxy->unregister_stream = &plugin_service_delegate::unregister_stream;
        proxy->destroy_stream = &plugin_service_delegate::destroy_stream;
        proxy->create_stream_bin = &plugin_service_delegate::create_stream_bin;
        proxy->destroy_stream_bin = &plugin_service_delegate::destroy_stream_bin;
        proxy->bin_has_connections = &plugin_service_delegate::bin_has_connections;
        proxy->cycle_bin_buffers = &plugin_service_delegate::cycle_bin_buffers;
        proxy->link_connection_to_bin = &plugin_service_delegate::link_connection_to_bin;
        proxy->get_parameter_bin = &plugin_service_delegate::get_parameter_bin;
        proxy->log = &plugin_service_delegate::log;
        proxy->pluginService = service;

        return proxy;
    }
}

#endif /* ASTRA_CREATE_PLUGIN_PROXY_H */
