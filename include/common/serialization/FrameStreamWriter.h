#ifndef FRAMESTREAMWRITER_H
#define FRAMESTREAMWRITER_H

#include <astra/astra.hpp>
#include <astra/capi/streams/stream_types.h>
#include <astra_core/capi/plugins/astra_plugin.h>

#include "StreamFileModels.h"
#include "../clock/Stopwatch.h"
#include "FrameOutputStream.h"

#include <memory>

namespace astra { namespace serialization {

    FrameOutputStream* open_frame_output_stream(FILE* file);
    void close_frame_output_stream(FrameOutputStream*& stream);

    class FrameStreamWriter
    {
    public:
        FrameStreamWriter(FrameOutputStream& frameOutputStream);
        ~FrameStreamWriter();

        bool begin_write();
        bool end_write();
        bool write(depthframe& depthFrame);

    private:
        void stage_frame(astra_frame_t& astraFrame);
        void stage_frame_description(astra_frame_t& astraFrame, double fps);
        void populate_frame(astra_frame_t& astraFrame, Frame& frame);
        void populate_frame_description(astra_frame_t& astraFrame, FrameDescription& frameDescription, double fps);

        FrameOutputStream& m_outputStream;
        bool m_shouldWrite{ false };

        Stopwatch m_swatch;
        std::string m_swatchName;
    };

}}

#endif /* FRAMESTREAMWRITER_H */
