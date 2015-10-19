#ifndef ASTRA_COLOR_HPP
#define ASTRA_COLOR_HPP

#include <astra_core/astra_core.hpp>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/color_capi.h>
#include <astra/streams/astra_image.hpp>

namespace astra {

    class colorstream : public imagestream
    {
    public:

        explicit colorstream(astra_streamconnection_t connection)
            : imagestream(connection)
        {
            colorStream_ = reinterpret_cast<astra_colorstream_t>(connection);
        }

        static const astra_stream_type_t id = ASTRA_STREAM_COLOR;


    private:
        astra_colorstream_t colorStream_;
    };

    class colorframe : public imageframe<rgb_pixel, ASTRA_STREAM_COLOR>
    {
    public:
        colorframe(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_RGB888)
        {}
    };

    class rawcolorframe : public imageframe<uint8_t, ASTRA_STREAM_COLOR>
    {
    public:
        rawcolorframe(astra_imageframe_t frame)
            : imageframe(frame, ASTRA_PIXEL_FORMAT_RGB888)
        {}
    };
}

#endif // ASTRA_COLOR_HPP
