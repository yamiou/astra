#ifndef DEVICEADAPTER_H
#define DEVICEADAPTER_H

#include <SenseKit.h>

namespace sensekit {

    enum driver_status_t
        {
            DRIVER_STATUS_SUCCESS
        };

    class DriverAdapter;

    using device_handle_t =  void*;
    using stream_handle_t = void*;

    enum stream_type_t
        {
            SENSEKIT_STREAM_COLOR = 1,
            SENSEKIT_STREAM_DEPTH = 2
        };

    using device_connected_callback_t = void (*)(DriverAdapter*, const sensekit_device_desc_t&, void*);
    using device_disconnected_callback_t = void (*)(const sensekit_device_desc_t&, void* context);
    using device_changed_callback_t = void (*)(const sensekit_device_desc_t&, void*);

    class DriverAdapter
    {
    public:
        DriverAdapter() {};
        virtual ~DriverAdapter() {}

        virtual sensekit_status_t initialize(
            device_connected_callback_t connectedCallback,
            device_disconnected_callback_t disconnectedCallback,
            device_changed_callback_t changedCallback,
            void* context)
            {
                m_deviceConnectedCallback = connectedCallback;
                m_deviceDisconnectedCallback = disconnectedCallback;
                m_deviceChangedCallback = changedCallback;
                m_context = context;

                return SENSEKIT_STATUS_SUCCESS;
            };

        virtual sensekit_status_t terminate() = 0;
        virtual device_handle_t open_device(const char* uri) = 0;
        virtual driver_status_t close_device(device_handle_t handle) = 0;
        virtual driver_status_t get_available_streams(
            device_handle_t deviceHandle,
            const sensekit_stream_desc_t* descArray,
            size_t* count) = 0;

        virtual stream_handle_t open_stream(device_handle_t deviceHandle, stream_type_t steamType) = 0;
        virtual void close_stream(device_handle_t deviceHandle, stream_handle_t streamHandle) = 0;
        virtual sensekit_status_t has_device_for_uri(const char *uri, bool& deviceAvailable) = 0;

    protected:

        device_connected_callback_t m_deviceConnectedCallback;
        device_disconnected_callback_t m_deviceDisconnectedCallback;
        device_changed_callback_t m_deviceChangedCallback;
        void* m_context;

    private:

    };
}

#endif /* DEVICEADAPTER_H */
