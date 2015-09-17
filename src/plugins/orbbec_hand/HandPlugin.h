#ifndef HAND_PLUGIN_H
#define HAND_PLUGIN_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include <unordered_map>
#include "HandTracker.h"
#include "HandSettings.h"

namespace astra { namespace plugins { namespace hand {

    class HandPlugin : public PluginBase
    {
    public:
        HandPlugin(PluginServiceProxy* pluginProxy);
        virtual ~HandPlugin();

        virtual void temp_update() override { }

    protected:
        virtual void on_initialize() override;

    private:
        static void stream_registered_handler_thunk(void* clientTag,
                                               astra_streamset_t setHandle,
                                               astra_stream_t streamHandle,
                                               astra_stream_desc_t desc);

        static void stream_unregistering_handler_thunk(void* clientTag,
                                                  astra_streamset_t setHandle,
                                                  astra_stream_t streamHandle,
                                                  astra_stream_desc_t desc);

        void stream_registered_handler(astra_streamset_t setHandle,
                                  astra_stream_t streamHandle,
                                  astra_stream_desc_t desc);
        void stream_unregistering_handler(astra_streamset_t setHandle,
                                     astra_stream_t streamHandle,
                                     astra_stream_desc_t desc);


        astra_callback_id_t m_streamAddedCallbackId{0};
        astra_callback_id_t m_streamRemovingCallbackId{0};

        using StreamTrackerMap =  std::unordered_map<astra_stream_t,
                                                     HandTracker*,
                                                     StreamHandleHash,
                                                     StreamHandleEqualTo>;

        StreamTrackerMap m_streamTrackerMap;

        HandSettings m_settings;
    };
}}}

#endif /* HAND_PLUGIN_H */
