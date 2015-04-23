#ifndef IMAGE_H
#define IMAGE_H

#include <SenseKit/SenseKit.h>
#include <stdexcept>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/image_capi.h>

namespace sensekit {

    struct RGBPixel : public sensekit_rgb_pixel_t
    {
        RGBPixel()
          : RGBPixel(0, 0, 0)
        {}

        RGBPixel(uint8_t r, uint8_t g, uint8_t b)
        {
            this->r = r;
            this->g = g;
            this->b = b;
        }
    };

    template<typename TDataType>
    class ImageFrame
    {
    public:
        ImageFrame(sensekit_reader_frame_t readerFrame,
                   sensekit_stream_type_t type,
                   sensekit_stream_subtype_t subtype)
        {
            if (readerFrame != nullptr)
            {
                sensekit_reader_get_imageframe(readerFrame, type, subtype, &m_imageFrame);
                if (m_imageFrame != nullptr)
                {
                    sensekit_imageframe_get_metadata(m_imageFrame, &m_metadata);
                    sensekit_imageframe_get_frameindex(m_imageFrame, &m_frameIndex);
                    sensekit_imageframe_get_stream(m_imageFrame, &m_streamConnection);

                    void* voidData = nullptr;
                    sensekit_imageframe_get_data_ptr(m_imageFrame, &voidData, &m_byteLength);
                    m_dataPtr = static_cast<TDataType*>(voidData);
                }
            }
        }

        bool is_valid() { return m_imageFrame != nullptr; }

        int resolutionX() { throwIfInvalidFrame(); return m_metadata.width; }
        int resolutionY() { throwIfInvalidFrame(); return m_metadata.height; }
        int bytesPerPixel() { throwIfInvalidFrame(); return m_metadata.bytesPerPixel; }

        sensekit_frame_index_t frameIndex() { throwIfInvalidFrame(); return m_frameIndex; }

        const TDataType* data() { throwIfInvalidFrame(); return m_dataPtr; }
        size_t byteLength() { throwIfInvalidFrame(); return m_byteLength; }
        size_t numberOfPixels() { throwIfInvalidFrame(); return m_metadata.width * m_metadata.height; }

        void copy_to(TDataType* buffer)
        {
            throwIfInvalidFrame();
            sensekit_imageframe_copy_data(m_imageFrame, buffer);
        }

    protected:
        sensekit_streamconnection_t streamHandle() { return m_streamConnection; }

    private:
        void throwIfInvalidFrame()
        {
            if (m_imageFrame == nullptr)
            {
                throw std::logic_error("Cannot operate on an invalid frame");
            }
        }

        sensekit_imageframe_t m_imageFrame{ nullptr };
        sensekit_image_metadata_t m_metadata;
        sensekit_frame_index_t m_frameIndex;
        sensekit_streamconnection_t m_streamConnection;

        TDataType* m_dataPtr;
        size_t m_byteLength;
    };
}

#endif // IMAGE_H
