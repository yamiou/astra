#ifndef SKELETONTRACKER_H
#define SKELETONTRACKER_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKit/SenseKit.h>
#include <SenseKitUL/skul_ctypes.h>
#include <SenseKitUL/streams/Depth.h>
#include <SenseKitUL/streams/skeleton_types.h>
#include "SkeletonStream.h"

namespace sensekit { namespace plugins { namespace skeleton {

    class SkeletonTracker : public sensekit::FrameReadyListener
    {
    public:
        static const size_t MAX_SKELETONS;

        SkeletonTracker(PluginServiceProxy& pluginService,
                        sensekit_streamset_t streamSet,
                        sensekit_stream_t sourceStream)
            : m_sourceStreamHandle(sourceStream),
              m_sensor(get_uri_for_streamset(pluginService, streamSet)),
              m_pluginService(pluginService)
        {
            m_reader = m_sensor.create_reader();
            m_depthStream = m_reader.stream<sensekit::DepthStream>();
            m_depthStream.start();

            m_reader.addListener(*this);
            m_skeletonStream = std::make_unique<SkeletonStream>(m_pluginService,
                                                                streamSet,
                                                                m_sourceStreamHandle,
                                                                SkeletonTracker::MAX_SKELETONS);
        }

        sensekit_stream_t sourceStream() { return m_sourceStreamHandle; }

        virtual void on_frame_ready(sensekit::StreamReader& reader, sensekit::Frame& frame) override;

    private:
        sensekit_stream_t m_sourceStreamHandle;
        DepthStream m_depthStream{nullptr};
        Sensor m_sensor;
        StreamReader m_reader;
        PluginServiceProxy& m_pluginService;

        using SkeletonStreamPtr = std::unique_ptr<SkeletonStream>;
        SkeletonStreamPtr m_skeletonStream;
    };


}}}


#endif /* SKELETONTRACKER_H */
