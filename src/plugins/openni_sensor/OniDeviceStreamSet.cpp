#include "OniDeviceStreamSet.h"

namespace sensekit { namespace plugins {

    OniDeviceStreamSet::OniDeviceStreamSet(PluginServiceProxy& pluginService,
                                           const openni::DeviceInfo* info)
        : m_logger(pluginService, "OniDeviceStreamSet"),
          m_pluginService(pluginService)
    {
        m_uri = info->getUri();
        m_pluginService.create_stream_set(m_streamSetHandle);
        m_sensor = std::make_unique<Sensor>(m_streamSetHandle);
    }

    OniDeviceStreamSet::~OniDeviceStreamSet()
    {
        m_pluginService.destroy_stream_set(m_streamSetHandle);
        close();
    }

    sensekit_status_t OniDeviceStreamSet::open()
    {
        if (m_isOpen)
            return SENSEKIT_STATUS_SUCCESS;

        m_logger.info("opening device: %s", m_uri.c_str());
        openni::Status rc =  m_oniDevice.open(m_uri.c_str());

        if (rc != openni::STATUS_OK)
        {
            m_logger.warn("failed to open device: %s", openni::OpenNI::getExtendedError());
            return SENSEKIT_STATUS_DEVICE_ERROR;
        }

        m_logger.warn("opened device: %s", m_uri.c_str());

        open_sensor_streams();

        m_isOpen = true;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OniDeviceStreamSet::close()
    {
        if (!m_isOpen)
            return SENSEKIT_STATUS_SUCCESS;

        close_sensor_streams();

        m_logger.info("closing oni device: %s", m_uri.c_str());
        m_oniDevice.close();

        m_isOpen = false;

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OniDeviceStreamSet::read()
    {
        if (!m_isOpen || m_streams.size() == 0)
            return SENSEKIT_STATUS_SUCCESS;

        int streamIndex = -1;
        int timeout = openni::TIMEOUT_NONE;

        if (openni::OpenNI::waitForAnyStream(m_oniStreams.data(),
                                             m_streams.size(),
                                             &streamIndex,
                                             timeout)
            == openni::STATUS_TIME_OUT)
        {
            return SENSEKIT_STATUS_TIMEOUT;
        }

        if (streamIndex == -1)
            return SENSEKIT_STATUS_TIMEOUT;

        m_streams[streamIndex]->read_frame();

        return SENSEKIT_STATUS_SUCCESS;
    }

    sensekit_status_t OniDeviceStreamSet::open_sensor_streams()
    {
        if (m_oniDevice.hasSensor(openni::SENSOR_COLOR))
        {
            OniColorStream* stream = new OniColorStream(m_pluginService,
                                                        *m_sensor,
                                                        m_oniDevice);

            sensekit_status_t rc = SENSEKIT_STATUS_SUCCESS;
            rc = stream->open();

            if (rc == SENSEKIT_STATUS_SUCCESS)
            {
                rc = stream->start();
                if (rc == SENSEKIT_STATUS_SUCCESS)
                {
                    m_streams.push_back (StreamPtr(stream));
                    m_oniStreams [m_streams.size() - 1] = stream->get_oni_stream();
                }
            }

            if ( rc != SENSEKIT_STATUS_SUCCESS)
                m_logger.warn("unable to open openni color stream.");
        }

        if (m_oniDevice.hasSensor(openni::SENSOR_DEPTH))
        {
            OniDepthStream* stream = new OniDepthStream(m_pluginService,
                                                        *m_sensor,
                                                        m_oniDevice);

            sensekit_status_t rc = SENSEKIT_STATUS_SUCCESS;
            rc = stream->open();

            if (rc == SENSEKIT_STATUS_SUCCESS)
            {
                rc = stream->start();
                if (rc == SENSEKIT_STATUS_SUCCESS)
                {
                    m_streams.push_back (StreamPtr(stream));
                    m_oniStreams [m_streams.size() - 1] = stream->get_oni_stream();
                }
            }

            if ( rc != SENSEKIT_STATUS_SUCCESS)
                m_logger.warn("unable to open openni depth stream.");
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
