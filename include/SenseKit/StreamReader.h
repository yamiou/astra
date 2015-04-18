#ifndef STREAMREADER_H
#define STREAMREADER_H

#include <SenseKit/sensekit_capi.h>
#include <SenseKit/FrameReadyListener.h>
#include <SenseKit/Frame.h>
#include <memory>
#include <vector>
#include <algorithm>
#include <functional>

namespace sensekit {

    class StreamReader
    {
    public:
        StreamReader()
        {}

        StreamReader(sensekit_reader_t reader)
            : m_readerRef(std::make_shared<ReaderRef>(reader))
            {}

        template<typename T>
        T stream()
            {
                return stream<T>(DEFAULT_SUBTYPE);
            }

        template<typename T>
        T stream(sensekit_stream_subtype_t subtype)
            {
                sensekit_streamconnection_t connection;

                sensekit_reader_get_stream(m_readerRef->get(),
                                           T::id,
                                           subtype,
                                           &connection);

                return T(connection);
            }

        void addListener(FrameReadyListener& listener)
            {
                m_readerRef.get()->addListener(listener);
            }

        void removeListener(FrameReadyListener& listener)
            {
                m_readerRef.get()->removeListener(listener);
            }

        bool is_valid() { return m_readerRef != nullptr; }

        Frame get_latest_frame(int timeoutMillis = SENSEKIT_TIMEOUT_FOREVER)
            {
                sensekit_reader_frame_t frame;
                sensekit_reader_open_frame(m_readerRef->get(), timeoutMillis, &frame);

                return Frame(frame);
            }

    private:

        class ReaderRef;
        using ReaderRefPtr = std::shared_ptr<ReaderRef>;

        StreamReader(ReaderRefPtr readerRef)
            : m_readerRef(readerRef)
            { }

        class ReaderRef :
            public std::enable_shared_from_this<ReaderRef>
        {
        public:
            ReaderRef(sensekit_reader_t reader)
                :  m_reader(reader)
                {
                    sensekit_reader_register_frame_ready_callback(m_reader,
                                                                  &ReaderRef::frame_ready_thunk,
                                                                  this,
                                                                  &m_callbackId);
                }

            ~ReaderRef()
                {
                    m_listeners.clear();
                    sensekit_reader_unregister_frame_ready_callback(&m_callbackId);
                    sensekit_reader_destroy(&m_reader);
                }

            static void frame_ready_thunk(void* clientTag,
                                          sensekit_reader_t reader,
                                          sensekit_reader_frame_t frame)
                {
                    ReaderRef* self = static_cast<ReaderRef*>(clientTag);
                    self->notify_listeners(frame);
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
                    StreamReader reader(shared_from_this());
                    for(FrameReadyListener& listener : m_listeners)
                    {
                        listener.on_frame_ready(reader, frameWrapper);
                    }
                    m_isNotifying = false;
                }

            sensekit_reader_t get() { return m_reader; }

        private:
            sensekit_reader_t m_reader;

            bool m_isNotifying{false};

            using ListenerList = std::vector<std::reference_wrapper<FrameReadyListener> >;

            ListenerList m_listeners;
            ListenerList m_addedListeners;
            ListenerList m_removedListeners;

            sensekit_reader_callback_id_t m_callbackId;
        };

        ReaderRefPtr m_readerRef;

        friend bool operator==(const StreamReader& lhs, const StreamReader& rhs);
    };

    inline bool operator==(const StreamReader& lhs, const StreamReader& rhs)
    {
        return lhs.m_readerRef == rhs.m_readerRef;
    }

    inline bool operator!=(const StreamReader& lhs, const StreamReader& rhs)
    {
        return !(lhs == rhs);
    }
}

#endif /* STREAMREADER_H */
