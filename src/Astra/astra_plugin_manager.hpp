#ifndef ASTRA_PLUGIN_MANAGER_H
#define ASTRA_PLUGIN_MANAGER_H

#include <string>
#include <vector>
#include <memory>
#include "astra_plugin_service.hpp"
#include "astra_shared_library.hpp"
#include <Astra/Plugins/PluginServiceProxyBase.h>
#include "astra_logger.hpp"

namespace astra {

    using initialize_fn = void(*)(PluginServiceProxyBase*);
    using terminate_fn = void(*)();
    using update_fn = void(*)();

    struct PluginFuncs
    {
        initialize_fn initialize{nullptr};
        terminate_fn terminate{nullptr};
        update_fn update{nullptr};
        process::lib_handle libHandle{nullptr};

        bool is_valid()
        {
            return initialize != nullptr &&
                terminate != nullptr &&
                update != nullptr &&
                libHandle != nullptr;
        }
    };

    class plugin_manager
    {
    public:
        plugin_manager(streamset_catalog& setCatalog);
        ~plugin_manager();

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

        using plugin_service_ptr = std::unique_ptr<plugin_service>;
        plugin_service_ptr m_pluginService;

        PluginServiceProxyBase* m_pluginServiceProxy{nullptr};
    };
}

#endif /* ASTRA_PLUGIN_MANAGER_H */
