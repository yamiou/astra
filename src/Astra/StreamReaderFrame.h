#ifndef STREAMREADERFRAME_H
#define STREAMREADERFRAME_H

#include <astra_types.h>

namespace astra {

    class StreamReaderFrame
    {
    public:
        StreamReaderFrame();
        ~StreamReaderFrame();

        astra_frame_ref_t* get_subframe(astra_stream_desc_t desc);
    private:
        astra_reader_frame_t* m_frame;
    };


}


#endif /* STREAMREADERFRAME_H */
