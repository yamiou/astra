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
            OniDeviceStreamSet(PluginServiceProxy& pluginService,
                               const openni::DeviceInfo* info)
                : m_pluginService(pluginService),
                  m_logger(pluginService)
                {
                    m_oniDevice.open(info->getUri());

                    m_uri = info->getUri();
                    m_pluginService.create_stream_set(m_streamSetHandle);
                    m_sensor = std::unique_ptr<Sensor>(new Sensor(m_streamSetHandle));

                    m_logger.info("creating device streamset");
                    open_sensor_streams();
                }



            ~OniDeviceStreamSet()
                {
                    close_sensor_streams();

                    m_logger.info("destroying device streamset (and oni device)");
                    m_pluginService.destroy_stream_set(m_streamSetHandle);
                    m_oniDevice.close();
                }

            sensekit_status_t read()
                {
                    if (m_streams.size() == 0)
                        return SENSEKIT_STATUS_SUCCESS;

                    int streamIndex = -1;
                    int timeout = ::openni::TIMEOUT_NONE;

                    if (::openni::OpenNI::waitForAnyStream(m_oniStreams.data(),
                                                           m_streams.size(),
                                                           &streamIndex,
                                                           timeout)
                        == ::openni::STATUS_TIME_OUT)
                    {
                        return SENSEKIT_STATUS_TIMEOUT;
                    }

                    if (streamIndex == -1)
                        return SENSEKIT_STATUS_TIMEOUT;

                    m_streams[streamIndex]->read_frame();

                    return SENSEKIT_STATUS_SUCCESS;
                }

            std::string get_uri() { return m_uri; }

        protected:
            inline sensekit::plugins::PluginLogger& get_logger() { return m_logger; }

        private:
            sensekit::plugins::PluginLogger m_logger;

            sensekit_status_t open_sensor_streams();
            sensekit_status_t close_sensor_streams();

            PluginServiceProxy& m_pluginService;
            std::unique_ptr<Sensor> m_sensor;
            sensekit_streamset_t m_streamSetHandle;
            openni::Device m_oniDevice;
            std::string m_uri;

            using StreamPtr = std::unique_ptr<OniDeviceStreamBase>;
            using StreamList = std::vector<StreamPtr>;

            StreamList m_streams;

            const static size_t MAX_ONI_STREAMS = 4;
            std::array<openni::VideoStream*, MAX_ONI_STREAMS> m_oniStreams;
        };
    }}

#endif /* ONIDEVICESTREAMSET_H */