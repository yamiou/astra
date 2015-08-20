#ifndef ONIDEVICESTREAMSET_H
#define ONIDEVICESTREAMSET_H

#include <Astra/Plugins/plugin_capi.h>
#include <Astra/Plugins/PluginLogger.h>
#include <OpenNI.h>
#include <memory>
#include <vector>
#include <string>
#include <array>
#include "OniDepthStream.h"
#include "OniColorStream.h"

namespace astra { namespace plugins {

    class OniDeviceStreamSet
    {
    public:
        OniDeviceStreamSet(std::string name, PluginServiceProxy& pluginService, const char* uri);
        ~OniDeviceStreamSet();

        astra_status_t open();
        astra_status_t close();
        astra_status_t read();

        std::string get_uri() { return m_uri; }

    private:
        bool m_isOpen{false};

        astra_status_t open_sensor_streams();
        astra_status_t close_sensor_streams();

        PluginServiceProxy& m_pluginService;
        std::unique_ptr<Sensor> m_sensor;
        astra_streamset_t m_streamSetHandle;
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
