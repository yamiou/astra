#ifndef ASTRA_FRAME_HPP
#define ASTRA_FRAME_HPP

#include <memory>
#include "capi/astra_core.h"

namespace astra {

    class frame
    {
    public:
        frame(astra_reader_frame_t readerFrame)
            : frame(readerFrame, true)
        { }

        frame(astra_reader_frame_t readerFrame, const bool autoCloseFrame)
            : frameRef_(std::make_shared<frame_ref>(readerFrame, autoCloseFrame))
        { }

        template<typename T>
        T get()
        {
            return get<T>(DEFAULT_SUBTYPE);
        }

        template<typename T>
        T get(astra_stream_subtype_t subtype)
        {
            return T::template acquire<T>(frameRef_->get_frame(), subtype);
        }

        bool is_valid()
        {
            return frameRef_->get_frame() != nullptr;
        }

        operator bool()
        {
            return is_valid();
        }

    private:
        class frame_ref
        {
        public:
            frame_ref(astra_reader_frame_t readerFrame, const bool autoCloseFrame)
                :  frame_(readerFrame),
                   autoCloseFrame_(autoCloseFrame)
            { }

            ~frame_ref()
            {
                if (frame_ != nullptr && autoCloseFrame_)
                {
                    astra_reader_close_frame(&frame_);
                }
            }

            astra_reader_frame_t get_frame() const { return frame_; }

        private:
            astra_reader_frame_t frame_;
            const bool autoCloseFrame_;
        };

        std::shared_ptr<frame_ref> frameRef_;
    };
}

#endif // ASTRA_FRAME_HPP
