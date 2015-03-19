#ifndef SENSEKITCONTEXT_H
#define SENSEKITCONTEXT_H

#include <SenseKit.h>
#include <atomic>
#include "PluginBase.h"
#include "PluginService.h"
#include "StreamRegistry.h"
#include "StreamContextFactory.h"

namespace sensekit {

    class SenseKitContext
    {
    public:
        SenseKitContext()
            : m_pluginService(*this) {}
        virtual ~SenseKitContext() {}

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        sensekit_status_t open_sensor(const char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t close_sensor(sensekit_sensor_t** sensor);
        sensekit_status_t open_depth_stream(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream);
        sensekit_status_t close_depth_stream(sensekit_depthstream_t** stream);
        sensekit_status_t open_depth_frame(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t*& frame);
        sensekit_status_t close_depth_frame(sensekit_depthframe_t*& frame);

        sensekit_status_t temp_update();

        StreamRegistry& get_streamRegistry() { return m_streamRegistry; }
        StreamContextFactory& get_contextFactory() { return m_contextFactory; }

    private:

        StreamContextFactory m_contextFactory;
        StreamRegistry m_streamRegistry;

        PluginService m_pluginService;
        PluginBase* m_plugin;

        sensekit_depthframe_t* m_currentFrame{ nullptr };

    };

    class PluginContext : public SenseKitContext
    {
    };
}

#endif /* SENSEKITCONTEXT_H */
