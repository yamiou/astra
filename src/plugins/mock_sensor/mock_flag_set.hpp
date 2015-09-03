#ifndef MOCK_FLAG_SET_H
#define MOCK_FLAG_SET_H

#include <set>
#include <string>
#include <iosfwd>

namespace orbbec { namespace mocks {

    class flag_set
    {
    public:
        void add_flag(const std::string& flag);
        void remove_flag(const std::string& flag);
        void set_flag(const std::string& flag, bool isSet);

        bool has_flag(const std::string& flag);
        bool has_flags() const { return flags_.size() > 0; }

    private:
        std::set<std::string> flags_;

        friend std::ostream& operator<<(std::ostream& os, const flag_set& fs);
    };
}}

#endif /* MOCK_FLAG_SET_H */
