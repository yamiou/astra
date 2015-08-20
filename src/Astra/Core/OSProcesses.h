#include <string>

namespace astra {
#if __ANDROID__
    std::string get_application_name();
    std::string get_application_filepath();
    std::string get_applictaion_libpath();
#endif
}
