#ifndef ASTRA_INFRARED_HPP
#define ASTRA_INFRARED_HPP

#include <astra_core/astra_core.hpp>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/infrared_capi.h>
#include <astra/streams/astra_image.hpp>
#include <astra/vector.hpp>

namespace astra {

    class infraredstream : public imagestream
    {
    public:
        explicit infraredstream(astra_streamconnection_t connection)
            : imagestream(connection)
        { }

        static const astra_stream_type_t id = ASTRA_STREAM_INFRARED;
    };

    class infraredframe_16 : public imageframe<uint16_t, ASTRA_STREAM_INFRARED>
    {
    public:
        infraredframe_16(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_GRAY16)
        {}
    };

    class infraredframe_rgb : public imageframe<rgb_pixel, ASTRA_STREAM_INFRARED>
    {
    public:
        infraredframe_rgb(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_RGB888)
        {}
    };

}

#endif /* ASTRA_INFRARED_HPP */
