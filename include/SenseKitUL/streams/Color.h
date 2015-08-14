#ifndef COLOR_H
#define COLOR_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/color_capi.h>
#include <SenseKitUL/streams/Image.h>

namespace sensekit {

    class ColorStream : public ImageStream
    {
    public:

        explicit ColorStream(sensekit_streamconnection_t connection)
            : ImageStream(connection)
        {
            m_colorStream = reinterpret_cast<sensekit_colorstream_t>(connection);
        }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_COLOR;


    private:
        sensekit_colorstream_t m_colorStream;
    };

    class ColorFrame : public ImageFrame<RGBPixel, SENSEKIT_STREAM_COLOR>
    {
    public:
        ColorFrame(sensekit_imageframe_t frame)
            : ImageFrame(frame)
        {}
    };

    class ColorFrameRaw : public ImageFrame<uint8_t, SENSEKIT_STREAM_COLOR>
    {
    public:
        ColorFrameRaw(sensekit_imageframe_t frame)
            : ImageFrame(frame)
        {}
    };
}

#endif // COLOR_H
