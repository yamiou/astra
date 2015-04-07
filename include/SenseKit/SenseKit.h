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
        sensekit_streamset_t m_pStreamSet;

        friend class StreamReader;
    };

    class FrameRef
    {
    public:
        FrameRef(sensekit_reader_frame_t readerFrame)
            : m_frame(readerFrame) { }

        template<typename T>
        T get()
            {
                return T(m_frame);
            }

        void release()
            {
                sensekit_reader_close_frame(&m_frame);
            }

    private:
        sensekit_reader_frame_t m_frame;
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
                return stream<T>(ANY_SUBTYPE);
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
                sensekit_reader_frame_t frame;
                sensekit_reader_open_frame(m_reader, timeoutMillis, &frame);
                return FrameRef(frame);
            }

    private:
        Sensor& m_sensor;
        sensekit_reader_t m_reader;
    };

    StreamReader Sensor::create_reader()
    {
        return StreamReader(*this);
    }

    class DataStream
    {
    public:
        DataStream(sensekit_streamconnection_t* connection)
            : m_connection(connection) {}

        void start()
            {
                sensekit_stream_start(m_connection);
            }
        void stop()
            {
                sensekit_stream_stop(m_connection);
            }

    private:
        sensekit_streamconnection_t* m_connection;
    };
}

#endif // SENSEKIT_H
