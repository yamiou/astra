#ifndef SENSEKIT_H
#define SENSEKIT_H

#include <stdexcept>
#include <memory>
#include <string>
#include <vector>

#include "sensekit_capi.h"

namespace sensekit {

    class StreamReader;

    class Sensor
    {
    public:
        Sensor(std::string uri)
            {
                sensekit_initialize(); //idempotent
                sensekit_streamset_open(uri.c_str(), &m_streamSet);
            }

        Sensor()
            : Sensor("") { }

        ~Sensor()
            {
                sensekit_streamset_close(&m_streamSet);
            }

        inline StreamReader create_reader();

    private:
        sensekit_streamset_t get_handle() { return m_streamSet; }

        sensekit_streamset_t m_streamSet;
        std::string m_uri;

        friend class StreamReader;
    };

    class Frame
    {
    public:
        Frame(sensekit_reader_frame_t readerFrame)
            : m_frame(std::make_shared<FrameRef>(readerFrame))
            { }

        template<typename T>
        T get()
            {
                return T(m_frame->get());
            }

    private:
        class FrameRef
        {
        public:
            FrameRef(sensekit_reader_frame_t readerFrame)
                :  m_frame(readerFrame) { }

            ~FrameRef()
                {
                    if (m_frame != nullptr)
                    {
                        sensekit_reader_close_frame(&m_frame);
                    }
                }

            sensekit_reader_frame_t get() { return m_frame; }

        private:
            sensekit_reader_frame_t m_frame;
        };

        std::shared_ptr<FrameRef> m_frame;
    };

    class StreamReader
    {
    public:
        StreamReader(sensekit_reader_t reader)
            : m_reader(std::make_shared<ReaderRef>(reader))
            { }

        template<typename T>
        T stream()
            {
                return stream<T>(ANY_SUBTYPE);
            }

        template<typename T>
        T stream(sensekit_stream_subtype_t subType)
            {
                sensekit_streamconnection_t connection;

                sensekit_reader_get_stream(m_reader->get(),
                                           T::id,
                                           subType,
                                           &connection);

                return T(connection);
            }

        Frame get_latest_frame(int timeoutMillis = SENSEKIT_TIMEOUT_FOREVER)
            {
                sensekit_reader_frame_t frame;
                sensekit_reader_open_frame(m_reader->get(), timeoutMillis, &frame);
                return Frame(frame);
            }

    private:
        class ReaderRef
        {
        public:
            ReaderRef(sensekit_reader_t reader)
                :  m_reader(reader) { }

            ~ReaderRef()
                {
                    if (m_reader != nullptr)
                    {
                        sensekit_reader_destroy(&m_reader);
                    }
                }

            sensekit_reader_t get() { return m_reader; }

        private:
            sensekit_reader_t m_reader;
        };

        std::shared_ptr<ReaderRef> m_reader;
    };

    StreamReader Sensor::create_reader()
    {
        sensekit_reader_t reader;
        sensekit_reader_create(get_handle(), &reader);

        return StreamReader(reader);
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
                    throw std::logic_error("Cannot start a stream that is not available");
                }
                sensekit_stream_start(m_connection);
            }
        void stop()
            {
                if (m_connection == nullptr)
                {
                    throw std::logic_error("Cannot stop a stream that is not available");
                }
                sensekit_stream_stop(m_connection);
            }

    private:
        sensekit_streamconnection_t m_connection;
    };
}

#endif // SENSEKIT_H
