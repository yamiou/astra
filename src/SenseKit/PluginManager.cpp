#include "PluginManager.h"
#include "vendor/tinydir.h"

namespace sensekit {

    PluginManager::PluginManager(StreamSetCatalog& catalog)
        : m_pluginService(std::make_unique<PluginService>(catalog))
    {
        m_pluginServiceProxy = m_pluginService->create_proxy();
    }

    PluginManager::~PluginManager()
    {
        unload_all_plugins();
        delete m_pluginServiceProxy;
    }

    void PluginManager::load_plugins(std::string searchPath)
    {
        STRACE("PluginManager", "load plugins");
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
        LibHandle libHandle = nullptr;
        STRACE("PluginManager", "try_load_plugin %s", path.c_str());

        os_load_library(path.c_str(), libHandle);

        PluginFuncs pluginFuncs;
        os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_initialize), (FarProc&)pluginFuncs.initialize);
        os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_terminate), (FarProc&)pluginFuncs.terminate);
        os_get_proc_address(libHandle, SK_STRINGIFY(sensekit_plugin_update), (FarProc&)pluginFuncs.update);
        pluginFuncs.libHandle = libHandle;

        if (pluginFuncs.is_valid())
        {
            STRACE("PluginManager", "try_load_plugin valid plugin");
            pluginFuncs.initialize(m_pluginServiceProxy);
            STRACE("PluginManager", "try_load_plugin initialized plugin");
            m_pluginList.push_back(pluginFuncs);
        }
        else
        {
            STRACE("PluginManager", "try_load_plugin invalid lib: init: %p, term: %p, update: %p",
                   pluginFuncs.initialize,
                   pluginFuncs.terminate,
                   pluginFuncs.update);

            os_free_library(libHandle);
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
            os_free_library(pluginFuncs.libHandle);
        }

        m_pluginList.clear();
    }

    void PluginManager::notify_host_event(sensekit_event_id id, const void* data, size_t dataSize)
    {
        m_pluginService->notify_host_event(id, data, dataSize);
    }
}
