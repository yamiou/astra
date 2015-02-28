#ifndef DEVICE_H
#define DEVICE_H

#include <vector>
#include "DriverAdapter.h"
#include "Stream.h"

class StreamSource;

namespace sensekit {

    class Device
    {
    public:

        typedef size_t device_id;

        Device(DriverAdapter& adapter, const sensekit_device_desc_t& desc)
            : m_driverAdapter(adapter), m_deviceDesc(desc), m_deviceHandle(nullptr)
            {}

        virtual ~Device() {}

        inline bool operator==(const Device& rhs)
            {
                return m_deviceHandle == rhs.m_deviceHandle;
            }

        const sensekit_device_desc_t& get_description() const { return m_deviceDesc; }
        device_id get_device_id() const { return device_id(m_deviceHandle); }
        bool is_open() const { return m_deviceHandle != nullptr; }

        Stream* open_stream();
        void close_stream(Stream* stream);
        void open();
        void close();

    private:

        typedef std::vector<StreamSource*> StreamSourceList;

        DriverAdapter& m_driverAdapter;
        StreamSourceList  m_streamSources;
        device_handle_t m_deviceHandle;

        const sensekit_device_desc_t& m_deviceDesc;
    };

}


#endif /* DEVICE_H */
