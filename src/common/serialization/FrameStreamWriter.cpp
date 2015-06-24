#include <common/serialization/FrameStreamWriter.h>
#include "ProtoFrameOutputStream.h"

namespace sensekit { namespace serialization {

    FrameOutputStream* open_frame_output_stream(FILE* file)
    {
#ifdef _MSC_VER
        int fileDescriptor = _fileno(file);
#else
        int fileDescriptor = fileno(file);
#endif

        ZeroCopyOutputStream* outputStream = new FileOutputStream(fileDescriptor);

        return new ProtoFrameOutputStream(outputStream);
    }

    FrameStreamWriter::FrameStreamWriter(FrameOutputStream& frameOutputStream):
        m_outputStream(frameOutputStream),
        m_swatchName("FrameStreamWriter")
    {
        m_swatch.set_mode(REAL_TIME);
    }

    FrameStreamWriter::~FrameStreamWriter()
    {
            
    }

    bool FrameStreamWriter::begin_write()
    {
        bool isSuccessful = true;

        StreamHeader streamHeader;
        streamHeader.frameType = 1;

        m_outputStream.stage_stream_header(&streamHeader);

        isSuccessful = m_outputStream.write_stream_header();

        if (isSuccessful)
        {
            m_shouldWrite = true;
            m_swatch.start(m_swatchName);
        }

        return isSuccessful;
    }

    bool FrameStreamWriter::end_write()
    {
        m_shouldWrite = false;

        return true;
    }

    bool FrameStreamWriter::write(DepthFrame& depthFrame)
    {
        bool isSuccessful = true;

        if (!m_shouldWrite)
        {
            return !isSuccessful;
        }

        m_swatch.stop(m_swatchName);

        stage_frame(depthFrame);
        stage_frame_description(depthFrame, m_swatch.get_time_so_far(m_swatchName));
            
        isSuccessful = m_outputStream.write_frame_description();
        isSuccessful = m_outputStream.write_frame();

        m_swatch.start(m_swatchName);

        return isSuccessful;
    }

    void FrameStreamWriter::stage_frame(DepthFrame& depthFrame)
    {
        Frame frame;
        populate_frame(depthFrame, frame);
        m_outputStream.stage_frame(&frame);
    }

    void FrameStreamWriter::stage_frame_description(DepthFrame& depthFrame, double fps)
    {
        FrameDescription frameDesc;
        populate_frame_description(depthFrame, frameDesc, fps);
        m_outputStream.stage_frame_description(&frameDesc);
    }

    void FrameStreamWriter::populate_frame(DepthFrame& depthFrame, Frame& frame)
    {
        sensekit_imageframe_t imageFrame = depthFrame.handle();
        sensekit_frame_t* sensekitFrame = imageFrame->frame;

        frame.frameIndex = sensekitFrame->frameIndex;
        frame.byteLength = sensekitFrame->byteLength;
        frame.rawFrameWrapper = sensekitFrame->data;
    }

    void FrameStreamWriter::populate_frame_description(DepthFrame& depthFrame, FrameDescription& frameDescription, double fps)
    {
        frameDescription.bufferLength = depthFrame.byteLength();
        frameDescription.framePeriod = fps;
    }
}}