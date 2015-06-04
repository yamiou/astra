#ifndef ONIDEVICESTREAMSET_H
#define ONIDEVICESTREAMSET_H

#include <SenseKit/Plugins/plugin_capi.h>
#include <SenseKit/Plugins/PluginLogger.h>
#include <OpenNI.h>
#include <memory>
#include <vector>
#include <string>
#include <array>
#include "OniDepthStream.h"
#include "OniColorStream.h"

namespace sensekit { namespace plugins {

    class OniDeviceStreamSet
    {
    public:
        OniDeviceStreamSet(PluginServiceProxy& pluginService, const char* uri);
        ~OniDeviceStreamSet();

        sensekit_status_t open();
        sensekit_status_t close();
        sensekit_status_t read();

        std::string get_uri() { return m_uri; }

    protected:
        inline sensekit::plugins::PluginLogger& get_logger() { return m_logger; }

    private:
        bool m_isOpen{false};

        sensekit::plugins::PluginLogger m_logger;

        sensekit_status_t open_sensor_streams();
        sensekit_status_t close_sensor_streams();

        PluginServiceProxy& m_pluginService;
        std::unique_ptr<Sensor> m_sensor;
        sensekit_streamset_t m_streamSetHandle;
        openni::Device m_oniDevice;
        std::string m_uri;

        using StreamPtr = std::unique_ptr<OniDeviceStreamBase>;
        using StreamPtrList = std::vector<StreamPtr>;

        StreamPtrList m_streams;

        const static size_t MAX_ONI_STREAMS = 4;
        std::array<openni::VideoStream*, MAX_ONI_STREAMS> m_oniStreams;
    };
}}

#endif /* ONIDEVICESTREAMSET_H */
