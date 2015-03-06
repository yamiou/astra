#ifndef DEVICE_H
#define DEVICE_H

#include <SenseKit.h>
#include <vector>
#include <unordered_map>
#include "DriverAdapter.h"

namespace sensekit {

    class Stream;
    class StreamSource;

    using StreamSourceList = std::vector<StreamSource*>;

    class Device
    {
    public:


        using device_id = size_t;

        Device(DriverAdapter& adapter, const sensekit_device_desc_t* desc)
            : m_driverAdapter(adapter),
              m_deviceDesc(desc),
              m_deviceHandle(nullptr),
              m_streamSources(nullptr),
              m_connected(false)
            { }

        virtual ~Device();


        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        const sensekit_device_desc_t& get_description() const { return *m_deviceDesc; }
        device_id get_device_id() const { return device_id(m_deviceHandle); }
        bool is_open() const { return m_deviceHandle != nullptr; }
        const bool& is_connected() const { return m_connected; }


        Stream* open_stream(sensekit_streamtype streamType);
        void close_stream(Stream* stream);

        void open();
        void close();

        StreamSourceList& get_stream_sources();

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

        DriverAdapter& m_driverAdapter;
        StreamSourceList* m_streamSources;

        device_handle_t m_deviceHandle;
        bool m_connected;
        const sensekit_device_desc_t* m_deviceDesc;
    };

}

#endif /* DEVICE_H */
