#ifndef MOCK_STREAM_H
#define MOCK_STREAM_H

#include <memory>

#include <astra_core/astra_core.hpp>
#include <astra_core/capi/plugins/astra_plugin.h>
#include <astra_core/Plugins/Stream.h>
#include <astra_core/Plugins/StreamBin.h>

#include "mock_stream_listener.hpp"

namespace orbbec { namespace mocks {

    class stream : public astra::plugins::Stream
    {
    public:
        inline stream(astra::PluginServiceProxy& pluginService,
                      astra_streamset_t streamSet,
                      astra::stream_description desc,
                      stream_listener& listener);

        inline astra_status_t read(astra_frame_index_t frameIndex);
        inline astra_status_t open();
        inline astra_status_t close();
        inline astra_status_t start();
        inline astra_status_t stop();

        bool is_open() const { return isOpen_; }
        bool is_started() const { return isStarted_; }

    protected:
        virtual astra_status_t on_read(astra_frame_index_t frameIndex)
        {
            return ASTRA_STATUS_SUCCESS;
        }

        virtual astra_status_t on_open() { return ASTRA_STATUS_SUCCESS; }
        virtual astra_status_t on_close() { return ASTRA_STATUS_SUCCESS; }
        virtual astra_status_t on_start() { return ASTRA_STATUS_SUCCESS; }
        virtual astra_status_t on_stop() { return ASTRA_STATUS_SUCCESS; }

    private:
        bool isOpen_{false};
        bool isStarted_{false};
        stream_listener& listener_;
    };
}}

namespace orbbec { namespace mocks {

    stream::stream(astra::PluginServiceProxy& pluginService,
                   astra_streamset_t streamSet,
                   astra::stream_description desc,
                   orbbec::mocks::stream_listener& listener)
        : Stream(pluginService,
                 streamSet,
                 desc),
          listener_(listener)
    {}

    astra_status_t stream::read(astra_frame_index_t frameIndex)
    {
        if (!is_open() || !is_started())
            return astra_status_t::ASTRA_STATUS_INVALID_OPERATION;

        return on_read(frameIndex);
    }

    astra_status_t stream::open()
    {
        if (is_open())
            return astra_status_t::ASTRA_STATUS_SUCCESS;

        auto rc = on_open();

        if (rc == astra_status_t::ASTRA_STATUS_SUCCESS)
        {
            isOpen_ = true;
            listener_.on_opened(this);
        }

        return rc;
    }

    astra_status_t stream::close()
    {
        if (!is_open())
            return astra_status_t::ASTRA_STATUS_SUCCESS;

        if (is_started())
        {
            auto rc = stop();
            if (rc != astra_status_t::ASTRA_STATUS_SUCCESS)
                return rc;
        }

        auto rc = on_close();

        if (rc == astra_status_t::ASTRA_STATUS_SUCCESS)
        {
            isOpen_ = false;
            listener_.on_closed(this);
        }

        return rc;
    }

    astra_status_t stream::start()
    {
        if (isStarted_)
            return astra_status_t::ASTRA_STATUS_SUCCESS;

        auto rc = on_start();

        if (rc == astra_status_t::ASTRA_STATUS_SUCCESS)
        {
            isStarted_ = true;
            listener_.on_started(this);
        }

        return rc;
    }

    astra_status_t stream::stop()
    {
        if (!isStarted_)
            return astra_status_t::ASTRA_STATUS_SUCCESS;

        auto rc = on_stop();

        if (rc == astra_status_t::ASTRA_STATUS_SUCCESS)
        {
            isStarted_ = false;
            listener_.on_stopped(this);
        }

        return rc;
    }
}}

#endif /* MOCK_STREAM_H */
