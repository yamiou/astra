#ifndef OPENNIADAPTER_H
#define OPENNIADAPTER_H

#include <OpenNI.h>
#include "../DriverAdapter.h"

namespace sensekit {

    class OpenNIAdapter : public DriverAdapter
    {
    public:
        OpenNIAdapter()
            : m_initialized(false),
              m_listener(*this)
            {}

        virtual ~OpenNIAdapter() {}

        virtual sensekit_status_t initialize() override;
        virtual sensekit_status_t terminate() override;

        virtual sensekit_status_t has_device_for_uri(const char* uri, bool& deviceAvailable) override;
        virtual void open_device(Device* device) override;
        virtual driver_status_t close_device(Device* device) override;
        virtual stream_handle_t open_stream(device_handle_t deviceHandle, stream_type_t streamType) override;
        virtual void close_stream(device_handle_t deviceHandle, stream_handle_t streamHandle) override;
        virtual driver_status_t get_available_streams(
            device_handle_t deviceHandle,
            const sensekit_stream_desc_t* descArray,
            size_t* count) override;

    private:

        class EventsListener :
            public openni::OpenNI::DeviceConnectedListener,
            public openni::OpenNI::DeviceDisconnectedListener,
            public openni::OpenNI::DeviceStateChangedListener
        {

            EventsListener(OpenNIAdapter& adapter) : m_adapter(adapter) { }
        public:

            virtual void onDeviceConnected(const openni::DeviceInfo* info) override
                {
                    m_adapter.on_device_connected(info);
                }

            virtual void onDeviceDisconnected(const openni::DeviceInfo* info) override
                {
                    m_adapter.on_device_disconnected(info);
                }

            virtual void onDeviceStateChanged(const openni::DeviceInfo* info,
                                         openni::DeviceState state) override
                {
                    m_adapter.on_device_changed(info, state);
                }

        private:
            OpenNIAdapter& m_adapter;

            friend class OpenNIAdapter;
        };

        void on_device_connected(const openni::DeviceInfo* info);
        void on_device_disconnected(const openni::DeviceInfo* info);
        void on_device_changed(const openni::DeviceInfo* info, openni::DeviceState state);

        void register_for_events();
        void unregister_for_events();

        void destroy_device(Device* device);

        EventsListener m_listener;

        bool m_initialized;

        openni::VideoStream m_colorStream;
        openni::VideoStream m_depthStream;

        friend class EventsListener;
    };
}


#endif // OPENNIADAPTER_H
