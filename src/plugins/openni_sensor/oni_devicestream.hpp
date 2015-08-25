#ifndef ONI_DEVICESTREAM_H
#define ONI_DEVICESTREAM_H

#include <Shiny.h>
#include <OpenNI.h>
#include <AstraUL/streams/image_parameters.h>
#include <AstraUL/streams/image_types.h>
#include <memory>
#include <cstring>
#include <vector>
#include "oni_mappers.hpp"
#include "oni_stream.hpp"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif

namespace orbbec { namespace ni {

    template<typename TFrameWrapper, typename TBufferBlockType>
    class devicestream : public stream
    {
    public:
        using wrapper_type = TFrameWrapper;
        using block_type = TBufferBlockType;

        devicestream(astra::PluginServiceProxy& pluginService,
                     astra_streamset_t streamSet,
                     astra::StreamDescription desc,
                     openni::Device& oniDevice,
                     openni::SensorType oniSensorType)
            : stream(pluginService,
                     streamSet,
                     desc),
              oniDevice_(oniDevice),
              oniSensorType_(oniSensorType)
        {
            PROFILE_FUNC();
        }

        virtual ~devicestream()
        {
            PROFILE_FUNC();
            close();
        }

        virtual astra_status_t open() override final
        {
            PROFILE_FUNC();
            if (isOpen_)
                return ASTRA_STATUS_SUCCESS;

            LOG_INFO("orbbec.ni.devicestream", "creating oni stream of type: %d", get_description().get_type());
            openni::Status rc = oniStream_.create(oniDevice_, oniSensorType_);

            if (rc != openni::STATUS_OK)
            {
                return ASTRA_STATUS_DEVICE_ERROR;
            }

            LOG_INFO("orbbec.ni.devicestream", "created oni stream of type: %d", get_description().get_type());

            const openni::SensorInfo& pInfo = oniStream_.getSensorInfo();
            auto& modes = pInfo.getSupportedVideoModes();

            LOG_INFO("orbbec.ni.devicestream", "stream type %d supports modes:", get_description().get_type());

            for(int i = 0; i < modes.getSize(); i++)
            {
                const openni::VideoMode& oniMode = modes[i];

                if (std::get<0>(convert_format(oniMode.getPixelFormat())) != 0)
                {
                    astra_imagestream_mode_t mode = convert_mode(oniMode);
                    mode.id = i+1;
                    modes_.push_back(mode);
                }

                LOG_INFO("orbbec.ni.devicestream", "- w: %d h: %d fps: %d pf: %d",
                      oniMode.getResolutionX(),
                      oniMode.getResolutionY(),
                      oniMode.getFps(),
                      oniMode.getPixelFormat());
            }

            oniVideoMode_ = oniStream_.getVideoMode();
            mode_ = convert_mode(oniVideoMode_);

            LOG_INFO("orbbec.ni.devicestream", "Selected mode: w: %d h: %d fps: %d pf: %d",
                  mode_.width,
                  mode_.height,
                  mode_.fps,
                  mode_.pixelFormat);

            assert(mode_.pixelFormat != 0);

            bufferLength_ =
                mode_.width *
                mode_.height *
                mode_.bytesPerPixel;

            bin_ = std::make_unique<bin_type>(pluginService(),
                                              get_handle(),
                                              bufferLength_);

            on_open();

            isOpen_ = true;

            enable_callbacks();
            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t close() override final
        {
            PROFILE_FUNC();
            if (!isOpen_)
                return ASTRA_STATUS_SUCCESS;

            stop();

            on_close();

            LOG_INFO("orbbec.ni.devicestream", "destroying oni stream of type: %d", get_description().get_type());
            oniStream_.destroy();

            isOpen_ = isStreaming_ = false;

            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t start() override final
        {
            PROFILE_FUNC();
            if (!isOpen_ || isStreaming_)
                return ASTRA_STATUS_SUCCESS;

            LOG_INFO("orbbec.ni.devicestream", "starting oni stream of type: %d", get_description().get_type());
            oniStream_.start();
            LOG_INFO("orbbec.ni.devicestream", "started oni stream of type: %d", get_description().get_type());

            isStreaming_ = true;

            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t stop() override final
        {
            PROFILE_FUNC();
            if (!isOpen_ || !isStreaming_)
                return ASTRA_STATUS_SUCCESS;

            LOG_INFO("orbbec.ni.devicestream", "stopping oni stream of type: %d", get_description().get_type());
            oniStream_.stop();
            LOG_INFO("orbbec.ni.devicestream", "stopped oni stream of type: %d", get_description().get_type());

            isStreaming_ = false;

            return ASTRA_STATUS_SUCCESS;
        }

        inline bool is_streaming() const { return isOpen_ && isStreaming_; }

        virtual void on_get_parameter(astra_streamconnection_t connection,
                                      astra_parameter_id id,
                                      astra_parameter_bin_t& parameterBin) override
        {
            PROFILE_FUNC();

            switch (id)
            {
            case ASTRA_PARAMETER_IMAGE_HFOV:
            {
                size_t resultByteLength = sizeof(float);

                astra_parameter_data_t parameterData;
                astra_status_t rc = pluginService().get_parameter_bin(resultByteLength,
                                                                          &parameterBin,
                                                                          &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    float* hFov = reinterpret_cast<float*>(parameterData);
                    *hFov = oniStream_.getHorizontalFieldOfView();
                }
                break;
            }
            case ASTRA_PARAMETER_IMAGE_VFOV:
            {
                size_t resultByteLength = sizeof(float);

                astra_parameter_data_t parameterData;
                astra_status_t rc = pluginService().get_parameter_bin(resultByteLength,
                                                                          &parameterBin,
                                                                          &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    float* vFov = reinterpret_cast<float*>(parameterData);
                    *vFov = oniStream_.getVerticalFieldOfView();
                }
                break;
            }
            case ASTRA_PARAMETER_IMAGE_MIRRORING:
            {
                size_t resultByteLength = sizeof(bool);

                astra_parameter_data_t parameterData;
                astra_status_t rc = pluginService().get_parameter_bin(resultByteLength,
                                                                          &parameterBin,
                                                                          &parameterData);
                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    bool mirroring = oniStream_.getMirroringEnabled();
                    *reinterpret_cast<bool*>(parameterData) = mirroring;
                }

                break;
            }
            case ASTRA_PARAMETER_IMAGE_MODES:
            {
                std::size_t resultSize = sizeof(astra_imagestream_mode_t) * modes_.size();

                astra_parameter_data_t parameterData;
                astra_status_t rc = pluginService().get_parameter_bin(resultSize,
                                                                          &parameterBin,
                                                                          &parameterData);

                astra_imagestream_mode_t* result = static_cast<astra_imagestream_mode_t*>(parameterData);

                if (rc == ASTRA_STATUS_SUCCESS)
                {
                    for(int i = 0; i < modes_.size(); i++)
                    {
                        result[i] = modes_[i];
                    }
                }

                break;
            }
            }
        }

        void on_set_parameter(astra_streamconnection_t connection,
                              astra_parameter_id id,
                              size_t inByteLength,
                              astra_parameter_data_t inData) override
        {
            switch (id)
            {
            case ASTRA_PARAMETER_IMAGE_MIRRORING:
            {
                bool enable = *static_cast<bool*>(inData);
                oniStream_.setMirroringEnabled(enable);
                break;
            }
            case ASTRA_PARAMETER_IMAGE_MODE:
                astra_imagestream_mode_t* mode = static_cast<astra_imagestream_mode_t*>(inData);
                auto oniMode = convert_mode(*mode);
                auto rc = oniStream_.setVideoMode(oniMode);

                if (rc == ::openni::STATUS_OK)
                {
                    LOG_INFO("orbbec.ni.devicestream", "stream mode changed");
                    oniVideoMode_ = oniMode;
                    mode_ = *mode;
                }
                break;
            }
        }

        virtual void on_new_buffer(wrapper_type* wrapper)
        {
            PROFILE_FUNC();

            if (!wrapper)
                return;

            auto& md = wrapper->frame.metadata;

            md.pixelFormat = mode_.pixelFormat;
            md.width = mode_.width;
            md.height = mode_.height;
            md.bytesPerPixel = mode_.bytesPerPixel;
        }

        virtual astra_status_t read_frame(astra_frame_index_t frameIndex) override;

        virtual openni::VideoStream* get_stream() override { return &oniStream_; }

        virtual void on_connection_added(astra_streamconnection_t connection) override;
        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override;

    protected:
        openni::Device& oniDevice_;
        openni::SensorType oniSensorType_;
        openni::VideoStream oniStream_;
        openni::VideoMode oniVideoMode_;
        astra_imagestream_mode_t mode_;

    private:
        virtual void on_open() {}
        virtual void on_close() {}

        bool isOpen_{false};
        bool isStreaming_{false};

        using bin_type = astra::plugins::StreamBin<wrapper_type>;
        std::unique_ptr<bin_type> bin_;

        size_t bufferLength_{0};

        astra_stream_t streamHandle_{nullptr};

        std::vector<astra_imagestream_mode_t> modes_;
    };

    template<typename TFrameWrapper, typename TBufferBlockType>
    void devicestream<TFrameWrapper,
                      TBufferBlockType>::on_connection_added(astra_streamconnection_t connection)
    {
        PROFILE_FUNC();
        assert(bin_ != nullptr);
        bin_->link_connection(connection);
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    void devicestream<TFrameWrapper,
                      TBufferBlockType>::on_connection_removed(astra_bin_t bin,
                                                               astra_streamconnection_t connection)
    {
        PROFILE_FUNC();
        bin_->unlink_connection(connection);

        if (!bin_->has_connections())
        {
            bin_ = nullptr;
        }
    }

    template<typename TFrameWrapper, typename TBufferBlockType>
    astra_status_t devicestream<TFrameWrapper, TBufferBlockType>::read_frame(astra_frame_index_t frameIndex)
    {
        PROFILE_FUNC();
        if (!is_streaming()) return ASTRA_STATUS_SUCCESS;

        openni::VideoFrameRef ref;
        PROFILE_BEGIN(oni_stream_readFrame);
        auto status = oniStream_.readFrame(&ref);
        PROFILE_END();

        if (status == ::openni::STATUS_OK)
        {
            const block_type* oniFrameData = static_cast<const block_type*>(ref.getData());

            size_t byteSize = MIN(ref.getDataSize(), bufferLength_);

            wrapper_type* wrapper = bin_->begin_write(frameIndex);

            wrapper->frame.frame = nullptr;
            wrapper->frame.data =
                reinterpret_cast<block_type*>(&(wrapper->frame_data));

            on_new_buffer(wrapper);

            std::memcpy(wrapper->frame.data, oniFrameData, byteSize);

            PROFILE_BEGIN(oni_stream_end_write);
            bin_->end_write();
            PROFILE_END();
        }

        return ASTRA_STATUS_SUCCESS;
    }
}}

#endif /* ONI_DEVICESTREAM_H */
