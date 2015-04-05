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
            static void stream_added_handler_thunk(StreamSetHandle* setHandle,
                                                   sensekit_stream_handle_t streamHandle,
                                                   sensekit_stream_desc_t desc);

            static void stream_removing_handler_thunk(StreamSetHandle* setHandle,
                                                      sensekit_stream_handle_t streamHandle,
                                                      sensekit_stream_desc_t desc);

            void stream_added_handler(StreamSetHandle* setHandle,
                                      sensekit_stream_handle_t streamHandle,
                                      sensekit_stream_desc_t desc);
            void stream_removing_handler(StreamSetHandle* setHandle,
                                         sensekit_stream_handle_t streamHandle,
                                         sensekit_stream_desc_t desc);

            CallbackId m_streamAddedCallbackId;
            CallbackId m_streamRemovingCallbackId;

            typedef std::unordered_map<sensekit_stream_handle_t, HandTracker*> StreamTrackerMap;
            StreamTrackerMap m_streamTrackerMap;
        };
    }
}

#endif /* HANDS_PLUGIN_H */
