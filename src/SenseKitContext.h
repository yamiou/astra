#ifndef SENSEKITCONTEXT_H
#define SENSEKITCONTEXT_H

#include <SenseKit.h>
#include <atomic>
#include "PluginBase.h"
#include "PluginService.h"
#include "StreamSetFactory.h"

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
        sensekit_status_t open_frame(sensekit_stream_t* stream, int timeout, sensekit_frame_t*& frame);
        sensekit_status_t close_frame(sensekit_stream_t* stream, sensekit_frame_t*& frame);

        sensekit_status_t temp_update();

        StreamSetFactory& get_contextFactory() { return m_contextFactory; }

    private:

        StreamSetFactory m_contextFactory;

        PluginService m_pluginService;
        PluginBase* m_plugin;
    };

    class PluginContext : public SenseKitContext
    {
    };
}

#endif /* SENSEKITCONTEXT_H */
