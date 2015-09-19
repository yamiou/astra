#include "PluginManager.h"
#include "vendor/tinydir.h"

namespace astra {

    PluginManager::PluginManager(StreamSetCatalog& catalog)
        : m_pluginService(std::make_unique<PluginService>(catalog)),
          m_pluginServiceProxy(m_pluginService->proxy())
    {}

    PluginManager::~PluginManager()
    {
        unload_all_plugins();
    }

    void PluginManager::load_plugins(std::string searchPath)
    {
        LOG_TRACE("PluginManager", "load plugins");
        std::vector<std::string> pluginFiles = find_libraries(searchPath);
        for(auto pluginPath : pluginFiles)
        {
            try_load_plugin(searchPath + pluginPath);
        }
    }

    void PluginManager::load_plugin(std::string pluginPath)
    {
        try_load_plugin(pluginPath);
    }

    std::vector<std::string> PluginManager::find_libraries(const std::string& pluginsPath)
    {

#ifdef _WIN32
        std::vector<std::string> extensions = { "dll" };
#else
        std::vector<std::string> extensions = { "so", "dylib" };
#endif //_WIN32

        std::vector<std::string> result;
        tinydir_dir dir;
        if (tinydir_open(&dir, pluginsPath.c_str()) == -1)
        {
            goto bail;
        }

        while (dir.has_next)
        {
            tinydir_file file;
            if (tinydir_readfile(&dir, &file) == -1)
            {
                goto bail;
            }

            if (!file.is_dir)
            {
                for(auto candidate : extensions)
                {
                    std::string extension(file.extension);
                    if (extension.compare(candidate) == 0)
                    {
                        result.push_back(std::string(file.name));
                        break;
                    }
                }
            }

            tinydir_next(&dir);
        }

    bail:
        tinydir_close(&dir);
        return result;
    }

    void PluginManager::try_load_plugin(const std::string& path)
    {
        process::lib_handle libHandle = nullptr;
        LOG_TRACE("PluginManager", "try_load_plugin %s", path.c_str());

        process::load_library(path.c_str(), libHandle);

	if (!libHandle)
	  return;

        PluginFuncs pluginFuncs;
        process::get_proc_address(libHandle, ASTRA_STRINGIFY(astra_plugin_initialize), (process::far_proc&)pluginFuncs.initialize);
        process::get_proc_address(libHandle, ASTRA_STRINGIFY(astra_plugin_terminate), (process::far_proc&)pluginFuncs.terminate);
        process::get_proc_address(libHandle, ASTRA_STRINGIFY(astra_plugin_update), (process::far_proc&)pluginFuncs.update);
        pluginFuncs.libHandle = libHandle;

        if (pluginFuncs.is_valid())
        {
            LOG_TRACE("PluginManager", "try_load_plugin valid plugin");
            pluginFuncs.initialize(m_pluginServiceProxy);
            LOG_TRACE("PluginManager", "try_load_plugin initialized plugin");
            m_pluginList.push_back(pluginFuncs);
        }
        else
        {
            LOG_TRACE("PluginManager", "try_load_plugin invalid lib: init: %p, term: %p, update: %p",
                   pluginFuncs.initialize,
                   pluginFuncs.terminate,
                   pluginFuncs.update);

            process::free_library(libHandle);
        }
    }

    void PluginManager::update()
    {
        for(auto plinfo : m_pluginList)
        {
            if (plinfo.update)
            {
                plinfo.update();
            }
        }
    }

    void PluginManager::unload_all_plugins()
    {
        for(auto pluginFuncs : m_pluginList)
        {
            pluginFuncs.terminate();
            process::free_library(pluginFuncs.libHandle);
        }

        m_pluginList.clear();
    }

    void PluginManager::notify_host_event(astra_event_id id, const void* data, size_t dataSize)
    {
        m_pluginService->notify_host_event(id, data, dataSize);
    }
}
