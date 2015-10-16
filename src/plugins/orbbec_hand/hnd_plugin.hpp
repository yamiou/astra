#ifndef HND_PLUGIN_H
#define HND_PLUGIN_H

#include <Astra/Plugins/PluginKit.h>
#include <AstraUL/AstraUL.h>
#include <map>
#include "hnd_hand_tracker.hpp"
#include "hnd_settings.hpp"

namespace astra { namespace hand {

    class plugin : public PluginBase
    {
    public:
        plugin(PluginServiceProxy* pluginProxy);
        virtual ~plugin();

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


        astra_callback_id_t streamAddedCallbackId_{0};
        astra_callback_id_t streamRemovingCallbackId_{0};

        using hand_tracker_map = std::map<astra_stream_t,
                                          hand_tracker*>;

        hand_tracker_map streamTrackerMap_;

        hand_settings settings_;
    };
}}

#endif /* HND_PLUGIN_H */
