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
#ifndef SKELETONPLUGIN_H
#define SKELETONPLUGIN_H

#include <astra_core/Plugins/PluginKit.h>
#include <astra/astra.hpp>
#include "SkeletonTracker.h"
#include <memory>
#include <vector>

namespace astra { namespace plugins { namespace skeleton {

    class SkeletonPlugin : public astra::PluginBase
    {
    public:
        static const size_t MAX_SKELETONS = 5;

        SkeletonPlugin(PluginServiceProxy* pluginProxy)
            : PluginBase(pluginProxy, "orbbec_skeleton")
        {
            register_for_stream_events();
        }

        virtual ~SkeletonPlugin()
        {
            unregister_for_stream_events();
        }

    private:
        virtual void on_stream_added(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t desc) override;

        virtual void on_stream_removed(astra_streamset_t setHandle,
                                       astra_stream_t streamHandle,
                                       astra_stream_desc_t desc) override;

        using SkeletonTrackerPtr = std::unique_ptr<SkeletonTracker>;
        using SkeletonTrackerList = std::vector<SkeletonTrackerPtr>;

        SkeletonTrackerList m_skeletonTrackers;
    };
}}}


#endif /* SKELETONPLUGIN_H */
