#!/bin/sh

cmake -G "Unix Makefiles" \
      -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_C_COMPILER=/usr/local/bin/clang-3.5 \
      -DCMAKE_CXX_COMPILER=/usr/local/bin/clang++-3.5 \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..

cp compile_commands.json ..
