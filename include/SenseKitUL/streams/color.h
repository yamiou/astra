#ifndef COLOR_H
#define COLOR_H

#include <SenseKit.h>
#include <StreamTypes.h>
#include "color_capi.h"

namespace sensekit {

    class ColorStream : public DataStream
    {
    public:
        explicit ColorStream(sensekit_streamconnection_t connection)
            : DataStream(connection)
            { }

        static const sensekit_stream_type_t id = SENSEKIT_STREAM_COLOR;
    };

    class ColorFrame
    {
    public:
        ColorFrame(sensekit_reader_frame_t readerFrame)
        {
            if (readerFrame != nullptr)
            {
                sensekit_color_frame_get(readerFrame, &m_colorFrame);
                sensekit_colorframe_get_metadata(m_colorFrame, &m_metadata);
                sensekit_colorframe_get_frameindex(m_colorFrame, &m_frameIndex);
                sensekit_colorframe_get_data_ptr(m_colorFrame, &m_dataPtr, &m_dataLength);
            }
        }

        bool is_valid() { return m_colorFrame != nullptr; }
        int get_resolutionX() { throwIfInvalidFrame(); return m_metadata.width; }
        int get_resolutionY() { throwIfInvalidFrame(); return m_metadata.height; }
        sensekit_frame_index_t get_frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }
        int get_bytesPerPixel() { throwIfInvalidFrame(); return m_metadata.bytesPerPixel; }

        const uint8_t* data() { throwIfInvalidFrame(); return m_dataPtr; }
        size_t length() { throwIfInvalidFrame(); return m_dataLength; }

        void copy_to(uint8_t* buffer)
            {
                throwIfInvalidFrame();
                sensekit_colorframe_copy_data(m_colorFrame, buffer);
            }

    private:
        void throwIfInvalidFrame()
        {
            if (m_colorFrame == nullptr)
            {
                throw std::exception("Cannot operate on an invalid frame");
            }
        }

        sensekit_colorframe_t m_colorFrame { nullptr };
        sensekit_colorframe_metadata_t m_metadata;
        sensekit_frame_index_t m_frameIndex;
        uint8_t* m_dataPtr;
        size_t m_dataLength;
    };
}

#endif // COLOR_H
