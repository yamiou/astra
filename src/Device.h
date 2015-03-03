#ifndef DEVICE_H
#define DEVICE_H

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

        Device(DriverAdapter& adapter, const sensekit_device_desc_t& desc)
            : m_driverAdapter(adapter),
              m_deviceDesc(desc),
              m_deviceHandle(nullptr)
            {}

        virtual ~Device() {}

        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        const sensekit_device_desc_t& get_description() const { return m_deviceDesc; }
        device_id get_device_id() const { return device_id(m_deviceHandle); }
        bool is_open() const { return m_deviceHandle != nullptr; }

        StreamDescriptionList get_available_streams();

        Stream* open_stream(stream_type_t streamType);
        void close_stream(Stream* stream);

        void open();
        void close();

    private:

        using StreamMap = std::unordered_map<stream_type_t, Stream*, std::hash<int> >;

        DriverAdapter& m_driverAdapter;
        StreamMap  m_streams;
        device_handle_t m_deviceHandle;

        const sensekit_device_desc_t& m_deviceDesc;
    };

}

#endif /* DEVICE_H */
