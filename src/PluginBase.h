#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "PluginService.h"

namespace sensekit
{
    class Context;

    class PluginBase
    {
    public:
        PluginBase() { };
        virtual ~PluginBase() { };

        //stream core calls these on plugins
        //TODO transition this init call to the PluginBase ctor
        void initialize(Context* context, PluginService* pluginService)
            {
                if (m_initialized)
                    return;

                m_frameworkContext = context;
                m_pluginService = pluginService;

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
        inline Context& get_context() const { return *m_frameworkContext; }
        inline PluginService& get_pluginService() const  { return *m_pluginService; }

        virtual void on_initialize() {}
        virtual void on_cleanup() {}

    private:
        bool m_initialized{false};
        Context* m_frameworkContext{nullptr};
        PluginService* m_pluginService{nullptr};
    };
}

#endif /* PLUGINBASE_H */