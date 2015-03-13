#ifndef STREAMSERVER_H
#define STREAMSERVER_H

#include <vector>
#include "Signal.h"

namespace sensekit {

    class Stream;
    class StreamSource;

    using StreamSourceList = std::vector<StreamSource*>;

    class StreamServer
    {
    public:
        StreamServer();
        virtual ~StreamServer();

        void initialize();
        void terminate();

        bool is_initialized() const { return m_initialized; }

        inline const StreamSourceList get_stream_sources() const
            {
                return m_sourceList;
            }

        inline Signal<StreamSource*> source_added_signal() const
            {
                return m_streamSourceAddedSignal;
            }

        inline Signal<StreamSource*> source_removed_signal() const
            {
                return m_streamSourceRemovedSignal;
            }

    protected:
        inline void raise_source_added(StreamSource* stream)
            {
                m_streamSourceAddedSignal.raise(stream);
            }

        inline void raise_source_removed(StreamSource* stream)
            {
                m_streamSourceRemovedSignal.raise(stream);
            }

        bool add_stream_source(StreamSource* source);
        bool remove_stream_source(StreamSource* source);
        void clear_sources();
        bool source_exists(StreamSource* source);

        virtual void on_initialize() = 0;
        virtual void on_terminate() = 0;

    private:
        bool m_initialized;

        Signal<StreamSource*> m_streamSourceAddedSignal;
        Signal<StreamSource*> m_streamSourceRemovedSignal;

        StreamSourceList m_sourceList;
    };
}

#endif /* STREAMSERVER_H */
