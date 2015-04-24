#include "OniDeviceStreamSet.h"

namespace sensekit { namespace plugins {

        sensekit_status_t OniDeviceStreamSet::open_sensor_streams()
        {
            ::openni::Status rc = ::openni::STATUS_OK;

            m_logger.info("opening depth stream");
            if (rc == ::openni::STATUS_OK)
            {
                OniDepthStream* stream = new OniDepthStream(m_pluginService,
                                                            *m_sensor,
                                                            m_oniDevice);
                stream->start();
                m_streams.push_back(StreamPtr(stream));
                m_oniStreams[m_streams.size() - 1] = stream->get_oni_stream();
            }
            else
            {
                m_logger.warn("Failed to open depth stream");
            }

            m_logger.info("opening color stream");

            if (rc == ::openni::STATUS_OK)
            {
                OniColorStream* stream = new OniColorStream(m_pluginService,
                                                            *m_sensor,
                                                            m_oniDevice);
                stream->start();
                m_streams.push_back(StreamPtr(stream));
                m_oniStreams[m_streams.size() - 1] = stream->get_oni_stream();
            }
            else
            {
                m_logger.warn("Failed to open color stream");
            }

            return SENSEKIT_STATUS_SUCCESS;
        }

        sensekit_status_t OniDeviceStreamSet::close_sensor_streams()
        {
            m_streams.clear();
            m_oniStreams.fill(nullptr);

            return SENSEKIT_STATUS_SUCCESS;
        }



    }}
