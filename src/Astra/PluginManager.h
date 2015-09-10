#ifndef PLUGINMANAGER_H
#define PLUGINMANAGER_H

#include <string>
#include <vector>
#include <memory>
#include "PluginService.h"
#include "shared_library.h"
#include <Astra/Plugins/PluginServiceProxyBase.h>
#include "Logger.h"

namespace astra {

    using initialize_fn = void(*)(PluginServiceProxyBase*);
    using terminate_fn = void(*)();
    using update_fn = void(*)();

    struct PluginFuncs
    {
        initialize_fn initialize{nullptr};
        terminate_fn terminate{nullptr};
        update_fn update{nullptr};
        LibHandle libHandle{nullptr};

        bool is_valid()
        {
            return initialize != nullptr &&
                terminate != nullptr &&
                update != nullptr;
        }
    };

    class PluginManager
    {
    public:
        PluginManager(StreamSetCatalog& setCatalog);
        ~PluginManager();

        void load_plugins(std::string searchPath);
        void load_plugin(std::string pluginPath);

        void update();
        void unload_all_plugins();
        size_t plugin_count() const { return m_pluginList.size(); }

        void notify_host_event(astra_event_id id, const void* data, size_t dataSize);
    private:
        std::vector<std::string> find_libraries(const std::string& pluginsPath);
        void try_load_plugin(const std::string& path);

        using PluginList = std::vector<PluginFuncs>;
        PluginList m_pluginList;

        using PluginServicePtr = std::unique_ptr<PluginService>;
        PluginServicePtr m_pluginService;

        PluginServiceProxyBase* m_pluginServiceProxy{nullptr};
    };
}

#endif /* PLUGINMANAGER_H */
