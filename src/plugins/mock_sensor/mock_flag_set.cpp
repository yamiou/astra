#include "mock_flag_set.hpp"
#include <iostream>

namespace orbbec { namespace mocks {

    void flag_set::add_flag(const std::string& flag)
    {
        if (flag.length() > 0)
            flags_.insert(flag);
    }

    void flag_set::remove_flag(const std::string& flag)
    {
        if (flag.length() > 0)
            flags_.erase(flag);
    }

    bool flag_set::has_flag(const std::string& flag)
    {
        return flags_.find(flag) != flags_.end();
    }

    void flag_set::set_flag(const std::string& flag, bool isSet)
    {
        if (isSet)
        {
            add_flag(flag);
        }
        else
        {
            remove_flag(flag);
        }
    }

    std::ostream& operator<<(std::ostream& os, const flag_set& fs)
    {
        for(auto& flag : fs.flags_)
        {
            os << flag << " ";
        }

        os.seekp(-1, std::ios_base::end);

        return os;
    }
}}
