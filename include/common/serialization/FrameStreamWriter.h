#ifndef FRAMESTREAMWRITER_H
#define FRAMESTREAMWRITER_H

#include <SensekitUL/SenseKitUL.h>
#include <SenseKitUL/Plugins/stream_types.h>
#include <SenseKit/Plugins/plugin_capi.h>

#include "StreamFileModels.h"
#include "../clock/Stopwatch.h"
#include "FrameOutputStream.h"

#include <memory>

namespace sensekit { namespace serialization {
    
    FrameOutputStream* open_frame_output_stream(FILE* file);

    class FrameStreamWriter
    {
    public:
        FrameStreamWriter(FrameOutputStream& frameOutputStream);
        ~FrameStreamWriter();

        bool begin_write();
        bool end_write();
        bool write(DepthFrame& depthFrame);

    private:
        void stage_frame(DepthFrame& depthFrame);
        void stage_frame_description(DepthFrame& depthFrame, double fps);
        void populate_frame(DepthFrame& depthFrame, Frame& frame);
        void populate_frame_description(DepthFrame& depthFrame, FrameDescription& frameDescription, double fps);

        FrameOutputStream& m_outputStream;
        bool m_shouldWrite{ false };

        Stopwatch m_swatch;
        std::string m_swatchName;
    };

}}

#endif /* FRAMESTREAMWRITER_H */