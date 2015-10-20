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
#ifndef SINGLEBINSTREAM_H
#define SINGLEBINSTREAM_H

#include "Stream.h"
#include "StreamBin.h"
#include <memory>

namespace astra { namespace plugins {

    template<typename TFrameType>
    class SingleBinStream : public astra::plugins::Stream
    {
    public:
        SingleBinStream(PluginServiceProxy& pluginService,
                        astra_streamset_t streamSet,
                        stream_description description,
                        size_t bufferSize)
            : Stream(pluginService,
                     streamSet,
                     description)
        {
            m_bin = std::make_unique<bin_type>(pluginService,
                                               get_handle(),
                                               sizeof(TFrameType) + bufferSize);
        }

        using frame_type = TFrameType;

        bool has_connections()
        {
            return m_bin->has_connections();
        }

        TFrameType* begin_write(size_t frameIndex)
        {
            return m_bin->begin_write(frameIndex);
        }

        void end_write()
        {
            return m_bin->end_write();
        }

    protected:
        virtual void on_connection_added(astra_streamconnection_t connection) override
        {
            m_bin->link_connection(connection);
        }

        virtual void on_connection_removed(astra_bin_t bin,
                                           astra_streamconnection_t connection) override
        {
            m_bin->unlink_connection(connection);
        }

    private:
        using bin_type = StreamBin<TFrameType>;
        std::unique_ptr<bin_type> m_bin;
    };
}}

#endif /* SINGLEBINSTREAM_H */
