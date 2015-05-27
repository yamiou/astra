#ifndef FRAME_H
#define FRAME_H

#include <memory>
#include <SenseKit/sensekit_capi.h>

namespace sensekit {

    class Frame
    {
    public:
        Frame(sensekit_reader_frame_t readerFrame)
            : m_frameRef(std::make_shared<FrameRef>(readerFrame))
        { }

        template<typename T>
        T get()
        {
            return get<T>(DEFAULT_SUBTYPE);
        }

        template<typename T>
        T get(sensekit_stream_subtype_t subtype)
        {
            return T::template acquire<T>(m_frameRef->get_frame(), subtype);
        }

        bool is_valid()
        {
            return m_frameRef->get_frame() != nullptr;
        }

        operator bool()
        {
            return is_valid();
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

            sensekit_reader_frame_t get_frame() { return m_frame; }

        private:
            sensekit_reader_frame_t m_frame;
        };

        std::shared_ptr<FrameRef> m_frameRef;
    };
}

#endif /* FRAME_H */
