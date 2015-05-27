#ifndef COLOR_H
#define COLOR_H

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/color_capi.h>
#include <SenseKitUL/streams/Image.h>

namespace sensekit {

    class ColorStream : public DataStream
    {
    public:
        ColorStream()
        {}

        explicit ColorStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
        {
            m_colorStream = reinterpret_cast<sensekit_colorstream_t>(connection);
        }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_COLOR;

        float horizontalFieldOfView()
        {
            float hFov;
            sensekit_colorstream_get_hfov(m_colorStream, &hFov);

            return hFov;
        }

        float verticalFieldOfView()
        {
            float vFov;
            sensekit_colorstream_get_vfov(m_colorStream, &vFov);

            return vFov;
        }

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
