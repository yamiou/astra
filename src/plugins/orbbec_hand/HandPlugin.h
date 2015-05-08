#ifndef HAND_PLUGIN_H
#define HAND_PLUGIN_H

#include <SenseKit/Plugins/PluginKit.h>
#include <SenseKitUL/SenseKitUL.h>
#include <unordered_map>
#include "handtracker.h"
#include "HandSettings.h"


namespace sensekit { namespace plugins { namespace hand {

    class StreamHandleHash
    {
    public:
        std::size_t operator()(const sensekit_stream_t streamHandle) const
        {
            return std::hash<sensekit_stream_t>()(streamHandle);
        }
    };

    class StreamHandleEqualTo
    {
    public:
        std::size_t operator()(const sensekit_stream_t& lhs,
                               const sensekit_stream_t& rhs) const
        {
            return lhs == rhs;
        }
    };

    class HandPlugin : public PluginBase
    {
    public:
        HandPlugin(PluginServiceProxy* pluginProxy);
        virtual ~HandPlugin();

        virtual void temp_update() override {}

    protected:
        virtual void on_initialize() override;

    private:
        static void stream_added_handler_thunk(void* clientTag,
                                               sensekit_streamset_t setHandle,
                                               sensekit_stream_t streamHandle,
                                               sensekit_stream_desc_t desc);

        static void stream_removing_handler_thunk(void* clientTag,
                                                  sensekit_streamset_t setHandle,
                                                  sensekit_stream_t streamHandle,
                                                  sensekit_stream_desc_t desc);

        void stream_added_handler(sensekit_streamset_t setHandle,
                                  sensekit_stream_t streamHandle,
                                  sensekit_stream_desc_t desc);
        void stream_removing_handler(sensekit_streamset_t setHandle,
                                     sensekit_stream_t streamHandle,
                                     sensekit_stream_desc_t desc);

        sensekit_callback_id_t m_streamAddedCallbackId;
        sensekit_callback_id_t m_streamRemovingCallbackId;

        using StreamTrackerMap =  std::unordered_map<sensekit_stream_t,
                                                     HandTracker*,
                                                     StreamHandleHash,
                                                     StreamHandleEqualTo>;

        StreamTrackerMap m_streamTrackerMap;

        HandSettings m_settings;
    };
}}}

#endif /* HAND_PLUGIN_H */
