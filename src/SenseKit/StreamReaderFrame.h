#ifndef STREAMREADERFRAME_H
#define STREAMREADERFRAME_H

#include <sensekit_types.h>

namespace sensekit {

    class StreamReaderFrame
    {
    public:
        StreamReaderFrame();
        ~StreamReaderFrame();

        sensekit_frame_ref_t* get_subframe(sensekit_stream_desc_t desc);
    private:
        sensekit_reader_frame_t* m_frame;
    };


}


#endif /* STREAMREADERFRAME_H */
