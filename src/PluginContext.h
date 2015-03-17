#ifndef PLUGINCONTEXT_H
#define PLUGINCONTEXT_H

#include <SenseKit.h>
#include "PluginBase.h"
#include "PluginService.h"

namespace sensekit {
    //suffix 2 just because old code -- GORDON?
    class Context2
    {
    public:
        virtual ~Context2() {}
    };

    class PluginContext : public Context2
    {
    public:
        PluginContext() {}
        virtual ~PluginContext() {}

        sensekit_status_t initialize();
        sensekit_status_t terminate();

        sensekit_status_t open_sensor(const char* uri, sensekit_sensor_t** sensor);
        sensekit_status_t close_sensor(sensekit_sensor_t** sensor);
        sensekit_status_t open_depth_stream(sensekit_sensor_t* sensor, sensekit_depthstream_t** stream);
        sensekit_status_t close_depth_stream(sensekit_depthstream_t** stream);
        sensekit_status_t open_depth_frame(sensekit_depthstream_t* stream, int timeout, sensekit_depthframe_t*& frame);
        sensekit_status_t close_depth_frame(sensekit_depthframe_t*& frame);

        sensekit_status_t temp_update();
    private:

        PluginService* m_pluginService;
        PluginBase* m_plugin;

        sensekit_depthframe_t* m_currentFrame{ nullptr };
    };
}

#endif /* PLUGINCONTEXT_H */
