#ifndef DIR_H
#define DIR_H

#include "../vendor/tinydir.h"
#include <vector>
#include <string>

namespace sensekit {

    std::vector<std::string> get_libs()
    {
#ifdef _WIN32
        std::vector<std::string> extensions = { "dll" };
#else
        std::vector<std::string> extensions = { "so", "dylib" };
#endif //_WIN32

        std::vector<std::string> result;
        tinydir_dir dir;
        if (tinydir_open(&dir, ".") == -1)
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
}

#endif /* DIR_H */
