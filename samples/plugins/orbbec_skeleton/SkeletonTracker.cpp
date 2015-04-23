#include "SkeletonTracker.h"

namespace sensekit { namespace plugins { namespace skeleton {

    const size_t SkeletonTracker::MAX_SKELETONS = 6;

    void SkeletonTracker::on_frame_ready(StreamReader& reader, Frame& frame)
    {
        if (!m_skeletonStream->has_connections())
            return; // don't waste cycles if no one is listening

        sensekit::DepthFrame depthFrame = frame.get<DepthFrame>();

        if (!depthFrame.is_valid())
            return;

        // do something cool
        sensekit_skeletonframe_wrapper_t* skeletonFrame = m_skeletonStream->begin_write(depthFrame.frameIndex());

        if (skeletonFrame != nullptr)
        {
            skeletonFrame->frame.skeletons = reinterpret_cast<sensekit_skeleton_t*>(&(skeletonFrame->frame_data));
            skeletonFrame->frame.skeletonCount = SkeletonTracker::MAX_SKELETONS;

            sensekit_skeleton_t& skeleton = skeletonFrame->frame.skeletons[0];
            skeleton.trackingId = 1;
            skeleton.status = SENSEKIT_SKELETON_STATUS_TRACKED;
            skeleton.jointCount = SENSEKIT_MAX_JOINTS;
            skeleton.joints[0].status = SENSEKIT_JOINT_STATUS_TRACKED;
            skeleton.joints[0].jointType = SENSEKIT_JOINT_TYPE_LEFT_HAND;
            skeleton.joints[0].position.x = 0;
            skeleton.joints[0].position.y = 0;
            skeleton.joints[0].position.z = 2000;

            for(int i = 1; i < MAX_SKELETONS; i++)
            {
                sensekit_skeleton_t& skeleton = skeletonFrame->frame.skeletons[i];
                skeleton.trackingId = i + 1;
                skeleton.status = SENSEKIT_SKELETON_STATUS_NOT_TRACKED;
            }

            m_skeletonStream->end_write();
        }
    }

}}}
