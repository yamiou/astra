#ifndef SENSOR_H
#define SENSOR_H

#include <string>
#include <SenseKit/sensekit_capi.h>
#include <Sensekit/StreamReader.h>
#include <stdexcept>

namespace sensekit {

    class Sensor
    {
    public:
        Sensor(std::string uri)
        {
            sensekit_streamset_open(uri.c_str(), &m_streamSetConnection);
            m_sensorRef = std::make_shared<SensorRef>(m_streamSetConnection);
        }

        Sensor(const sensekit_streamsetconnection_t& streamSetHandle)
            : m_streamSetConnection(streamSetHandle),
              m_sensorRef(std::make_shared<SensorRef>(streamSetHandle))
        {
            if (streamSetHandle == nullptr)
            {
                throw std::invalid_argument("streamSetHandle must not be null");
            }
        }

        Sensor()
        { }

        bool is_valid() { return m_sensorRef != nullptr; }

        inline StreamReader create_reader();
        sensekit_streamsetconnection_t get_handle() const { return m_streamSetConnection; }

    private:
        sensekit_streamsetconnection_t m_streamSetConnection;
        std::string m_uri;

        class SensorRef;
        using SensorRefPtr = std::shared_ptr<SensorRef>;

        class SensorRef :
            public std::enable_shared_from_this<SensorRef>
        {
        public:
            SensorRef(sensekit_streamsetconnection_t connection)
                :  m_connection(connection)
            { }

            ~SensorRef()
            {
                sensekit_streamset_close(&m_connection);
            }

            sensekit_streamsetconnection_t get_connection() { return m_connection; }

        private:
            sensekit_streamsetconnection_t m_connection;
        };

        SensorRefPtr m_sensorRef;

        friend bool operator==(const Sensor& lhs, const Sensor& rhs);
    };

    inline bool operator==(const Sensor& lhs, const Sensor& rhs)
    {
        return lhs.m_sensorRef == rhs.m_sensorRef;
    }


    StreamReader Sensor::create_reader()
    {
        sensekit_reader_t reader;
        sensekit_reader_create(get_handle(), &reader);

        return StreamReader(reader);
    }
}

#endif /* SENSOR_H */
