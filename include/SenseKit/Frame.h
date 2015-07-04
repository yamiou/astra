#ifndef FRAME_H
#define FRAME_H

#include <memory>
#include <SenseKit/sensekit_capi.h>

namespace sensekit {

    class Frame
    {
    public:
        Frame(sensekit_reader_frame_t readerFrame)
            : Frame(readerFrame, true)
        { }

        Frame(sensekit_reader_frame_t readerFrame, const bool autoCloseFrame)
            : m_frameRef(std::make_shared<FrameRef>(readerFrame, autoCloseFrame))
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
            FrameRef(sensekit_reader_frame_t readerFrame, const bool autoCloseFrame)
                :  m_frame(readerFrame),
                   m_autoCloseFrame(autoCloseFrame)
            { }

            ~FrameRef()
            {
                if (m_frame != nullptr && m_autoCloseFrame)
                {
                    sensekit_reader_close_frame(&m_frame);
                }
            }

            sensekit_reader_frame_t get_frame() const { return m_frame; }

        private:
            sensekit_reader_frame_t m_frame;
            const bool m_autoCloseFrame;
        };

        std::shared_ptr<FrameRef> m_frameRef;
    };
}

#endif /* FRAME_H */
