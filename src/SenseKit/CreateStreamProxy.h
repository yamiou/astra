/* THIS FILE AUTO-GENERATED FROM CreateStreamProxy.h.lpp. DO NOT EDIT. */
#ifndef CREATESTREAMPROXY_H
#define CREATESTREAMPROXY_H

#include "SenseKitContext.h"
#include "StreamServiceDelegate.h"
#include <Plugins/StreamServiceProxyBase.h>

namespace sensekit {

    static inline StreamServiceProxyBase* create_stream_proxy(SenseKitContext* context)
    {
        StreamServiceProxyBase* proxy = new StreamServiceProxyBase;

        proxy->initialize = &StreamServiceDelegate::initialize;
        proxy->terminate = &StreamServiceDelegate::terminate;
        proxy->streamset_open = &StreamServiceDelegate::streamset_open;
        proxy->streamset_close = &StreamServiceDelegate::streamset_close;
        proxy->get_status_string = &StreamServiceDelegate::get_status_string;
        proxy->reader_create = &StreamServiceDelegate::reader_create;
        proxy->reader_destroy = &StreamServiceDelegate::reader_destroy;
        proxy->reader_get_stream = &StreamServiceDelegate::reader_get_stream;
        proxy->stream_get_description = &StreamServiceDelegate::stream_get_description;
        proxy->stream_start = &StreamServiceDelegate::stream_start;
        proxy->stream_stop = &StreamServiceDelegate::stream_stop;
        proxy->reader_open_frame = &StreamServiceDelegate::reader_open_frame;
        proxy->reader_close_frame = &StreamServiceDelegate::reader_close_frame;
        proxy->reader_get_frame = &StreamServiceDelegate::reader_get_frame;
        proxy->stream_set_parameter = &StreamServiceDelegate::stream_set_parameter;
        proxy->stream_get_parameter_size = &StreamServiceDelegate::stream_get_parameter_size;
        proxy->stream_get_parameter_data = &StreamServiceDelegate::stream_get_parameter_data;
        proxy->temp_update = &StreamServiceDelegate::temp_update;
        proxy->streamService = context;

        return proxy;
    }
}

#endif /* CREATESTREAMPROXY_H */
