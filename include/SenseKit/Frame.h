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
                return T(m_frameRef->get());
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

        std::shared_ptr<FrameRef> m_frameRef;
    };
}

#endif /* FRAME_H */
