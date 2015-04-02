#ifndef HANDS_PLUGIN_H
#define HANDS_PLUGIN_H

#include <Plugins/PluginBase.h>
#include <unordered_map>
#include <SenseKitUL.h>
#include "handtracker.h"

namespace sensekit
{
    namespace hands
    {
        class HandsPlugin : public PluginBase
        {
        public:
            HandsPlugin(PluginServiceProxy* pluginProxy);
            virtual ~HandsPlugin();

            virtual void temp_update() override {}

        private:
            static void stream_added_handler_thunk(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType type, StreamSubtype subtype);
            static void stream_removing_handler_thunk(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType type, StreamSubtype subtype);

            void stream_added_handler(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType type, StreamSubtype subtype);
            void stream_removing_handler(StreamSetHandle* setHandle, StreamHandle* streamHandle, StreamType type, StreamSubtype subtype);

            CallbackId m_streamAddedCallbackId;
            CallbackId m_streamRemovingCallbackId;

            typedef std::unordered_map<StreamHandle*, HandTracker*> StreamTrackerMap;
            StreamTrackerMap m_streamTrackerMap;
        };
    }
}

EXPORT_PLUGIN(sensekit::hands::HandsPlugin);

#endif /* HANDS_PLUGIN_H */
