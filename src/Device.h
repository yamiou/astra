#ifndef DEVICE_H
#define DEVICE_H

#include <SenseKit.h>
#include <vector>
#include <unordered_map>
#include "DriverAdapter.h"

namespace sensekit {

    class DeviceStreamSource;
    class Stream;

    class Device
    {
    public:

        using device_id = size_t;
        using StreamDescriptionList = std::vector<sensekit_stream_desc_t>;

        Device(DriverAdapter& adapter, const sensekit_device_desc_t* desc)
            : m_driverAdapter(adapter),
              m_deviceDesc(desc),
              m_deviceHandle(nullptr),
              m_connected(false)
            { }

        virtual ~Device()
        {
            delete m_deviceDesc;
        }

        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        const sensekit_device_desc_t& get_description() const { return *m_deviceDesc; }
        device_id get_device_id() const { return device_id(m_deviceHandle); }
        bool is_open() const { return m_deviceHandle != nullptr; }
        const bool& is_connected() const { return m_connected; }
        StreamDescriptionList get_available_streams();

        Stream* open_stream(stream_type_t streamType);
        void close_stream(Stream* stream);

        void open();
        void close();

        void set_handle(device_handle_t handle)
            {
                m_deviceHandle = handle;
            }

        device_handle_t get_handle() const { return m_deviceHandle; }

        void on_connected()
            {
                m_connected = true;
            }

        void on_disconnected()
            {
                m_connected = false;
                set_handle(nullptr);
            }

        void on_changed() {}

    private:

        using StreamMap = std::unordered_map<stream_type_t, Stream*, std::hash<int> >;

        DriverAdapter& m_driverAdapter;
        StreamMap  m_streams;
        device_handle_t m_deviceHandle;
        bool m_connected;
        const sensekit_device_desc_t* m_deviceDesc;
    };

}

#endif /* DEVICE_H */
