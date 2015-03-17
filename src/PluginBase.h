#ifndef PLUGINBASE_H
#define PLUGINBASE_H

#include "SenseKit-private.h"

#include "PluginService.h"
#include "MockContext.h"
#include <OpenNI.h>

namespace sensekit
{
	class PluginBase
	{
	public:
		virtual ~PluginBase() { };

		//stream core calls these on plugins
		virtual void orbbec_plugin_init(MockContext* context, PluginService* pluginService) = 0;
		virtual void orbbec_plugin_cleanup() = 0;
		virtual void temp_update() = 0;
	};
}


#endif /* PLUGINBASE_H */