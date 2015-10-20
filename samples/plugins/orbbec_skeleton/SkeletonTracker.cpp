// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#include "SkeletonTracker.h"

namespace astra { namespace plugins { namespace skeleton {

    const size_t SkeletonTracker::MAX_SKELETONS = 6;

    void SkeletonTracker::on_frame_ready(stream_reader_& reader, frame& frame)
    {
        if (!skeletonStream_->has_connections())
            return; // don't waste cycles if no one is listening

        astra::depthframe depthFrame = frame.get<depthframe>();

        if (!depthFrame.is_valid())
            return;

        // do something cool
        astra_skeletonframe_wrapper_t* skeletonFrame = skeletonStream_->begin_write(depthFrame.frameIndex());

        if (skeletonFrame != nullptr)
        {
            skeletonFrame->frame.skeletons = reinterpret_cast<astra_skeleton_t*>(&(skeletonFrame->frame_data));
            skeletonFrame->frame.skeletonCount = SkeletonTracker::MAX_SKELETONS;

            astra_skeleton_t& skeleton = skeletonFrame->frame.skeletons[0];
            skeleton.trackingId = 1;
            skeleton.status = ASTRA_SKELETON_STATUS_TRACKED;
            skeleton.jointCount = ASTRA_MAX_JOINTS;
            skeleton.joints[0].status = ASTRA_JOINT_STATUS_TRACKED;
            skeleton.joints[0].jointType = ASTRA_JOINT_TYPE_LEFT_HAND;
            skeleton.joints[0].position.x = 0;
            skeleton.joints[0].position.y = 0;
            skeleton.joints[0].position.z = 2000;

            for(int i = 1; i < MAX_SKELETONS; i++)
            {
                astra_skeleton_t& skeleton = skeletonFrame->frame.skeletons[i];
                skeleton.trackingId = i + 1;
                skeleton.status = ASTRA_SKELETON_STATUS_NOT_TRACKED;
            }

            skeletonStream_->end_write();
        }
    }

}}}
