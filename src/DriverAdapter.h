#ifndef DEVICEADAPTER_H
#define DEVICEADAPTER_H

#include <SenseKit.h>

namespace sensekit {

    enum driver_status_t
        {
            DRIVER_STATUS_SUCCESS
        };

    class DriverAdapter;

    typedef void* device_handle_t;
    typedef void (*device_connected_callback_t)(DriverAdapter* adapter, device_handle_t deviceHandle, void* callbackContext);
typedef void (*device_disconnected_callback_t)(device_handle_t deviceHandle, void* callbackContext);

    class DriverAdapter
    {
    public:
        DriverAdapter() {}
        virtual ~DriverAdapter() {}

        virtual sensekit_status_t initialize(
            device_connected_callback_t connectedCallback,
            device_disconnected_callback_t disconnectedCallback,
            void* callbackContext)
            {
                m_deviceConnectedCallback = connectedCallback;
                m_deviceDisconnectedCallback = disconnectedCallback;
                m_callbackContext = callbackContext;

                return SENSEKIT_STATUS_SUCCESS;
            }
        virtual sensekit_status_t terminate() = 0;
        virtual device_handle_t open_device(char* uri) = 0;
        virtual driver_status_t close_device(device_handle_t handle) = 0;
        virtual sensekit_status_t has_device_for_uri(char *uri, bool& deviceAvailable) = 0;
    protected:
        device_connected_callback_t m_deviceConnectedCallback;
        device_disconnected_callback_t m_deviceDisconnectedCallback;
        void* m_callbackContext;

    private:

    };

}

#endif /* DEVICEADAPTER_H */
