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
#ifndef SKELETONSTREAM_H
#define SKELETONSTREAM_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra_core/Astra.h>
#include <astra/capi/astra_ctypes.h>
#include <astra/streams/skeleton_types.h>

namespace astra { namespace plugins { namespace skeleton {

    class skeletonstream : public astra::plugins::SingleBinStream<astra_skeletonframe_wrapper_t,
                                                                  astra_skeleton_joint_t>
    {
    public:
        skeletonstream(astra::PluginServiceProxy& pluginService,
                       astra_streamset_t streamSet,
                       astra_stream_t sourceStream,
                       size_t skeletonCount)
            : SingleBinStream(pluginService,
                              streamSet,
                              astra::stream_description(ASTRA_STREAM_SKELETON,
                                                        DEFAULT_SUBTYPE),
                              sizeof(astra_skeleton_t) * skeletonCount)

        {
            enable_callbacks();
        }
    };
}}}

#endif /* SKELETONSTREAM_H */
