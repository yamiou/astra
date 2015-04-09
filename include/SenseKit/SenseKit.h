#ifndef SENSEKIT_H
#define SENSEKIT_H

#include <stdexcept>
#include <memory>
#include <string>
#include <vector>
#include <functional>
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

        friend class StreamReader;
    };

    class StreamDescription : private sensekit_stream_desc_t
    {
    public:
        sensekit_stream_type_t get_Type() { return type; }
        sensekit_stream_subtype_t get_subType() { return subType; }

        friend class DataStream;
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

    class FrameReadyListener
    {
    public:
        virtual ~FrameReadyListener() = default;
        virtual void on_frame_ready(StreamReader& reader, Frame& frame) = 0;
    };

    inline bool operator==(const FrameReadyListener& l, const FrameReadyListener& r)
    {
        return &l == &r;
    }

    class StreamReader
    {
    public:
        StreamReader(sensekit_reader_t reader)
            : m_reader(std::make_shared<ReaderRef>(reader))
            {
                sensekit_reader_register_frame_ready_callback(m_reader->get(),
                                                              &StreamReader::frame_ready_thunk,
                                                              this,
                                                              &m_callbackId);
            }

        ~StreamReader()
            {
                m_listeners.clear();
                sensekit_reader_unregister_frame_ready_callback(&m_callbackId);
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

                sensekit_reader_get_stream(m_reader->get(),
                                           T::id,
                                           subType,
                                           &connection);

                return T(connection);
            }

        void addListener(FrameReadyListener& listener)
            {
                auto it = std::find(m_listeners.begin(),
                                    m_listeners.end(),
                                    listener);

                if (it != m_listeners.end())
                    return;

                if (m_isNotifying)
                {
                    m_addedListeners.push_back(listener);
                }
                else
                {
                    m_listeners.push_back(listener);
                }
            }

        void removeListener(FrameReadyListener& listener)
            {
                auto it = std::find(m_listeners.begin(),
                                    m_listeners.end(),
                                    listener);

                if (it == m_listeners.end())
                    return;

                if (m_isNotifying)
                {
                    m_removedListeners.push_back(listener);
                }
                else
                {
                    m_listeners.erase(it);
                }
            }

        Frame get_latest_frame(int timeoutMillis = SENSEKIT_TIMEOUT_FOREVER)
            {
                sensekit_reader_frame_t frame;
                sensekit_reader_open_frame(m_reader->get(), timeoutMillis, &frame);

                return Frame(frame);
            }

    private:
        static void frame_ready_thunk(void* clientTag,
                                      sensekit_reader_t reader,
                                      sensekit_reader_frame_t frame)
            {
                StreamReader* self = static_cast<StreamReader*>(clientTag);
                self->notify_listeners(frame);
            }

        void notify_listeners(sensekit_reader_frame_t readerFrame)
            {
                if (m_removedListeners.size() > 0)
                {
                    for(FrameReadyListener& listener : m_removedListeners)
                    {
                        auto it = std::find(m_listeners.begin(),
                                            m_listeners.end(),
                                            listener);

                        m_listeners.erase(it);
                    }
                    m_removedListeners.clear();
                }

                std::move(m_addedListeners.begin(),
                          m_addedListeners.end(),
                          std::back_inserter(m_listeners));

                if (m_listeners.size() == 0)
                    return;

                Frame frameWrapper(readerFrame);

                m_isNotifying = true;
                for(FrameReadyListener& listener : m_listeners)
                {
                    listener.on_frame_ready(*this, frameWrapper);
                }
                m_isNotifying = false;
            }

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

        bool m_isNotifying{false};

        using ListenerList = std::vector<std::reference_wrapper<FrameReadyListener> >;

        ListenerList m_listeners;
        ListenerList m_addedListeners;
        ListenerList m_removedListeners;

        std::shared_ptr<ReaderRef> m_reader;
        sensekit_reader_callback_id_t m_callbackId;
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
            : m_connection(connection)
            {
                sensekit_stream_get_description(connection, &m_description);
        }

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
        const StreamDescription& get_description()
            {
                return static_cast<const StreamDescription&>(m_description);
            }

    private:
        sensekit_streamconnection_t m_connection;
        sensekit_stream_desc_t m_description;
    };
}

#endif // SENSEKIT_H
