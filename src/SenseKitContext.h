#ifndef SENSEKITCONTEXT_H
#define SENSEKITCONTEXT_H

#include <SenseKit.h>
#include <atomic>
#include "plugins/PluginBase.h"
#include "PluginService.h"
#include "StreamSetFactory.h"
#include "StreamSet.h"

namespace sensekit {

    class SenseKitContext
    {
    public:
        SenseKitContext()
            : m_pluginService(*this) {}
        virtual ~SenseKitContext() {}

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        sensekit_status_t open_streamset(const char* uri, sensekit_streamset_t*& streamset);
        sensekit_status_t close_streamset(sensekit_streamset_t*& streamset);
        sensekit_status_t open_stream(sensekit_streamset_t* streamset, sensekit_stream_t*& stream);
        sensekit_status_t close_stream(sensekit_stream_t*& stream);
        sensekit_status_t open_frame(sensekit_stream_t* stream, int timeout, sensekit_frame_ref_t*& frameRef);
        sensekit_status_t close_frame(sensekit_frame_ref_t*& frameRef);

        sensekit_status_t temp_update();

        StreamSetFactory& get_setFactory() { return m_setFactory; }
        StreamSet& get_rootSet() { return m_rootSet; }

        sensekit_status_t set_parameter(sensekit_stream_t* stream, sensekit_parameter_id parameterId, size_t byteLength, sensekit_parameter_data_t* data);
        sensekit_status_t get_parameter_size(sensekit_stream_t* stream, sensekit_parameter_id parameterId, /*out*/size_t& byteLength);
        sensekit_status_t get_parameter_data(sensekit_stream_t* stream, sensekit_parameter_id parameterId, size_t byteLength, /*out*/sensekit_parameter_data_t*& data);
    private:

        StreamSetFactory m_setFactory;
        StreamSet m_rootSet{0};

        PluginService m_pluginService;
        PluginBase* m_plugin;
    };

    class PluginContext : public SenseKitContext
    {
    };
}

#endif /* SENSEKITCONTEXT_H */
