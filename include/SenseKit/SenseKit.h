#ifndef SENSEKIT_H
#define SENSEKIT_H

#include "sensekit_core.h"
#include <exception>
#include <memory>

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
            : m_data(new Data(readerFrame))
        { }

        ~FrameRef()
        {
            
        }
        FrameRef(const FrameRef& other)
        {
            m_data = other.m_data;
        }

        FrameRef& operator=(const FrameRef& other)
        {
            m_data = other.m_data;
            return *this;
        }

        template<typename T>
        T get()
            {
                return T(m_data->get_data());
            }

    private:
        class Data
        {
        public:
            Data(sensekit_reader_frame_t readerFrame) :
                m_frame(readerFrame)
            { }
            ~Data()
            {
                if (m_frame != nullptr)
                {
                    sensekit_reader_close_frame(&m_frame);
                }
            }
            sensekit_reader_frame_t get_data() { return m_frame; }

        private:
            sensekit_reader_frame_t m_frame;
        };

        std::shared_ptr<Data> m_data;
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
                sensekit_streamconnection_t connection;

                sensekit_reader_get_stream(m_reader,
                                           T::id,
                                           subType,
                                           &connection);

                return T(connection);
            }

        FrameRef get_latest_frame(int timeoutMillis = SENSEKIT_TIMEOUT_FOREVER)
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
        DataStream(sensekit_streamconnection_t connection)
            : m_connection(connection) {}

        bool is_available() { return m_connection != nullptr; }
        void start()
            {
                if (m_connection == nullptr)
                {
                    throw new std::exception("Cannot start a stream that is not available");
                }
                sensekit_stream_start(m_connection);
            }
        void stop()
            {
                if (m_connection == nullptr)
                {
                    throw new std::exception("Cannot stop a stream that is not available");
                }
                sensekit_stream_stop(m_connection);
            }

    private:
        sensekit_streamconnection_t m_connection;
    };
}

#endif // SENSEKIT_H
