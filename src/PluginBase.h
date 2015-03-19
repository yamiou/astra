#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginService.h"

namespace sensekit
{
    class SenseKitContext;

    class PluginBase
    {
    public:
        PluginBase(SenseKitContext& context, PluginService& pluginService)
            : m_frameworkContext(context),
              m_pluginService(pluginService)
            {};

        virtual ~PluginBase() = default;

        //stream core calls these on plugins
        //TODO transition this init call to the PluginBase ctor
        void initialize()
            {
                if (m_initialized)
                    return;

                on_initialize();

                m_initialized = true;
            }

        void cleanup()
            {
                if (!m_initialized)
                    return;

                on_cleanup();

                m_initialized = false;
            }

        virtual void temp_update() = 0;
        bool is_initialized() const { return m_initialized; }

    protected:
        inline SenseKitContext& get_context() const { return m_frameworkContext; }
        inline PluginService& get_pluginService() const  { return m_pluginService; }

        virtual void on_initialize() {}
        virtual void on_cleanup() {}

    private:
        bool m_initialized{false};
        SenseKitContext& m_frameworkContext;
        PluginService& m_pluginService;
    };
}

#endif /* PLUGINBASE_H */