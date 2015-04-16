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
                sensekit_streamset_open(uri.c_str(), &m_streamSet);
            }

        Sensor(const sensekit_streamset_t& streamSetHandle) : m_streamSet(streamSetHandle)
            {
                if (streamSetHandle == nullptr)
                {
                    throw std::invalid_argument("streamSetHandle must not be null");
                }
            }

        Sensor()
            : Sensor("") { }

        ~Sensor()
            {
                sensekit_streamset_close(&m_streamSet);
            }

        inline StreamReader create_reader();
        sensekit_streamset_t get_handle() const { return m_streamSet; }

    private:
        sensekit_streamset_t m_streamSet;
        std::string m_uri;

    };

    StreamReader Sensor::create_reader()
    {
        sensekit_reader_t reader;
        sensekit_reader_create(get_handle(), &reader);

        return StreamReader(reader);
    }
}

#endif /* SENSOR_H */
