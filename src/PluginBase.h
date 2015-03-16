#include "SenseKit-private.h"

#include "PluginService.h"
#include "MockContext.h"

namespace sensekit
{
	class PluginBase
	{
	public:
		//stream core calls these on plugins
		void orbbec_plugin_init(MockContext* context, PluginService* pluginService);
	};
}