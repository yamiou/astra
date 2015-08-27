#include "oni_device_streamset.hpp"
#include "oni_depthstream.hpp"
#include "oni_colorstream.hpp"
#include "oni_infrared_stream.hpp"
#include <Shiny.h>

namespace orbbec { namespace ni {

    device_streamset::device_streamset(std::string name,
                                       astra::PluginServiceProxy& pluginService,
                                       const char* uri)
        : pluginService_(pluginService),
          uri_(uri)
    {
        PROFILE_FUNC();
        uri_ = uri;
        pluginService_.create_stream_set(name.c_str(), streamSetHandle_);
    }

    device_streamset::~device_streamset()
    {
        PROFILE_FUNC();
        close();
        pluginService_.destroy_stream_set(streamSetHandle_);
    }

    astra_status_t device_streamset::open()
    {
        PROFILE_FUNC();
        if (isOpen_)
            return ASTRA_STATUS_SUCCESS;

        LOG_INFO("orbbec.ni.device_streamset", "opening device: %s", uri_.c_str());
        openni::Status rc =  oniDevice_.open(uri_.c_str());

        if (rc != openni::STATUS_OK)
        {
            LOG_WARN("orbbec.ni.device_streamset", "failed to open device: %s", openni::OpenNI::getExtendedError());
            return ASTRA_STATUS_DEVICE_ERROR;
        }

        LOG_INFO("orbbec.ni.device_streamset", "opened device: %s", uri_.c_str());

        open_sensor_streams();

        isOpen_ = true;

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t device_streamset::close()
    {
        PROFILE_FUNC();
        if (!isOpen_)
            return ASTRA_STATUS_SUCCESS;

        close_sensor_streams();

        LOG_INFO("orbbec.ni.device_streamset", "closing oni device: %s", uri_.c_str());
        oniDevice_.close();

        isOpen_ = false;

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t device_streamset::read()
    {
        PROFILE_BLOCK(streamset_read);
        if (!isOpen_ || streams_.size() == 0)
            return ASTRA_STATUS_SUCCESS;

        int streamIndex = -1;
        int timeout = openni::TIMEOUT_NONE;

        openni::Status rc;
        int i = 0;

        do
        {
            rc = openni::OpenNI::waitForAnyStream(oniStreams_.data(),
                                                  streams_.size(),
                                                  &streamIndex,
                                                  timeout);

            if (streamIndex != -1) streams_[streamIndex]->read_frame(frameIndex_);
            i++;
            if (streamIndex == 0)
            {
                //only increment frameIndex with primary stream
                //TODO this won't work when streams have different target FPS
                frameIndex_++;
            }
        } while (i < streams_.size() && rc == openni::STATUS_OK);

        if (rc == openni::STATUS_TIME_OUT)
        {
            return ASTRA_STATUS_TIMEOUT;
        }

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t device_streamset::start_stream(stream* stream)
    {
        astra_status_t rc = ASTRA_STATUS_SUCCESS;

        if (rc == ASTRA_STATUS_SUCCESS)
        {
            rc = stream->start();
            if (rc == ASTRA_STATUS_SUCCESS)
            {
                streams_.push_back(stream_ptr(stream));
                oniStreams_[streams_.size() - 1] = stream->get_stream();
            }
        }

        return rc;
    }

    astra_status_t device_streamset::open_sensor_streams()
    {
        PROFILE_FUNC();

        bool enableColor = true;
        if (enableColor && oniDevice_.hasSensor(openni::SENSOR_COLOR))
        {
            colorstream* stream = new colorstream(pluginService_,
                                                  streamSetHandle_,
                                                  oniDevice_);

            astra_status_t rc = ASTRA_STATUS_SUCCESS;
            rc = stream->open();

            if (stream->is_open())
            {
                rc = start_stream(stream);
            }

            if ( rc != ASTRA_STATUS_SUCCESS)
                LOG_WARN("orbbec.ni.device_streamset", "unable to open openni color stream.");
        }

        if (oniDevice_.hasSensor(openni::SENSOR_DEPTH))
        {
            depthstream* stream = new depthstream(pluginService_,
                                                  streamSetHandle_,
                                                  oniDevice_);

            astra_status_t rc = ASTRA_STATUS_SUCCESS;
            rc = stream->open();

            if (stream->is_open())
            {
                rc = start_stream(stream);
            }

            if (rc != ASTRA_STATUS_SUCCESS)
                LOG_WARN("orbbec.ni.device_streamset", "unable to open openni depth stream.");
        }

        if (oniDevice_.hasSensor(openni::SENSOR_IR))
        {
            infrared_stream* stream = new infrared_stream(pluginService_,
                                                          streamSetHandle_,
                                                          oniDevice_);

            astra_status_t rc = ASTRA_STATUS_SUCCESS;
            rc = stream->open();

            if (rc != ASTRA_STATUS_SUCCESS)
                LOG_WARN("orbbec.ni.device_streamset", "unable to open openni infrared stream.");
        }

        return ASTRA_STATUS_SUCCESS;
    }

    astra_status_t device_streamset::close_sensor_streams()
    {
        PROFILE_FUNC();
        streams_.clear();
        oniStreams_.fill(nullptr);

        return ASTRA_STATUS_SUCCESS;
    }
}}
