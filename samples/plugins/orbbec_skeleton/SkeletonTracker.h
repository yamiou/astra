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
#ifndef SKELETONTRACKER_H
#define SKELETONTRACKER_H

#include <astra_core/plugins/astra_plugin.hpp>
#include <astra_core/Astra.h>
#include <astra/capi/astra_ctypes.h>
#include <astra/streams/Depth.h>
#include <astra/streams/skeleton_types.h>
#include "skeletonstream.h"

namespace astra { namespace plugins { namespace skeleton {

    class SkeletonTracker : public astra::frame_listener
    {
    public:
        static const size_t MAX_SKELETONS;

        SkeletonTracker(pluginservice_proxy& pluginService,
                        astra_streamset_t streamSet,
                        astra_stream_t sourceStream)
            : sourceStreamHandle_(sourceStream),
              sensor_(get_uri_for_streamset(pluginService, streamSet)),
              pluginService_(pluginService)
        {
            reader_ = sensor_.create_reader();
            depthStream_ = reader_.stream<astra::depthstream>();
            depthStream_.start();

            reader_.addListener(*this);
            skeletonStream_ = std::make_unique<skeletonstream>(pluginService_,
                                                               streamSet,
                                                               sourceStreamHandle_,
                                                               SkeletonTracker::MAX_SKELETONS);
        }

        astra_stream_t sourceStream() { return sourceStreamHandle_; }

        virtual void on_frame_ready(astra::StreaReader_& reader, astra::frame& frame) override;

    private:
        astra_stream_t sourceStreamHandle_;
        depthstream depthStream_{nullptr};
        streamset sensor_;
        stream_reader reader_;
        pluginservice_proxy& pluginService_;

        using skeletonstream_ptr = std::unique_ptr<skeletonstream>;
        skeletonstream_ptr skeletonStream_;
    };


}}}


#endif /* SKELETONTRACKER_H */
