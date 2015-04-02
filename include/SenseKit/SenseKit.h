#ifndef SENSEKIT_H
#define SENSEKIT_H

#include "sensekit_core.h"

namespace sensekit {

    class StreamReader;

    class Sensor
    {
    public:
        Sensor()
            {
                sensekit_streamset_open("", &m_pStreamSet);
            }

        ~Sensor()
            {
                sensekit_streamset_close(&m_pStreamSet);
            }

        inline StreamReader create_reader();
    private:
        sensekit_streamset_t* m_pStreamSet;

        friend class StreamReader;
    };

    class FrameRef
    {
    public:
        FrameRef() { }

        template<typename T>
        T get()
            {
                return T();
            }

        void release()
            {

            }

    private:
    };

    class StreamReader
    {
    public:
        explicit StreamReader(Sensor& sensor)
            : m_sensor(sensor)
            {
                sensekit_reader_create(sensor.m_pStreamSet, &m_reader);
            }

        template<typename T>
        T stream()
            {
                return stream<T>(DEFAULT_SUBTYPE);
            }

        template<typename T>
        T stream(sensekit_stream_subtype_t subType)
            {
                sensekit_streamconnection_t* connection;

                sensekit_reader_get_stream(m_reader,
                                           T::id,
                                           subType,
                                           &connection);

                return T(connection);
            }

        FrameRef get_latest_frame(int timeoutMillis = -1)
            {
                sensekit_temp_update();
                return FrameRef();
            }

    private:
        Sensor& m_sensor;
        sensekit_reader_t* m_reader;
    };

    StreamReader Sensor::create_reader()
    {
        return StreamReader(*this);
    }
}

#endif // SENSEKIT_H
