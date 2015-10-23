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
#ifndef ORBBEC_SKELETON_TRACKER_HPP
#define ORBBEC_SKELETON_TRACKER_HPP

#include <astra_core/plugins/astra_plugin.hpp>
#include <astra/astra.hpp>
#include <astra/capi/astra_ctypes.h>
#include <astra/capi/streams/skeleton_types.h>
#include "orbbec_skeletonstream.hpp"

namespace orbbec { namespace skeleton {

    class skeleton_tracker : public astra::frame_listener
    {
    public:
        static const size_t MAX_SKELETONS;

        skeleton_tracker(astra::pluginservice_proxy& pluginService,
                         astra_streamset_t streamSet,
                         astra_stream_t sourceStream)
            : sourceStreamHandle_(sourceStream),
              sensor_(astra::plugins::get_uri_for_streamset(pluginService, streamSet)),
              reader_(sensor_.create_reader()),
              pluginService_(pluginService)
        {
            depthStream_ = reader_.stream<astra::depthstream>();
            depthStream_.start();

            reader_.addListener(*this);
            auto s = astra::plugins::make_stream<skeletonstream>(pluginService_,
                                                                 streamSet,
                                                                 skeleton_tracker::MAX_SKELETONS);

            skeletonStream_ = std::unique_ptr<skeletonstream>(std::move(s));
        }

        astra_stream_t sourceStream() { return sourceStreamHandle_; }

        virtual void on_frame_ready(astra::stream_reader& reader, astra::frame& frame) override;

    private:
        astra_stream_t sourceStreamHandle_;
        astra::depthstream depthStream_{nullptr};
        astra::streamset sensor_;
        astra::stream_reader reader_;
        astra::pluginservice_proxy& pluginService_;

        using skeletonstream_ptr = std::unique_ptr<skeletonstream>;
        skeletonstream_ptr skeletonStream_;
    };
}}

#endif /* ORBBEC_SKELETON_TRACKER_HPP */
