# This script locates the SenseKit library
# ------------------------------------
#
# Usage
# -----
# find_package(SenseKit REQUIRED) // find the graphics, window and system modules
#
# Output
# ------
#
# This script defines the following variables:
# - SENSEKIT_LIBRARIES:    the list of all libraries corresponding to the required modules
# - SENSEKIT_FOUND:        true if all the required modules are found
# - SENSEKIT_INCLUDE_DIR:  the path where SenseKit headers are located (the directory containing the SenseKit/SenseKit.h file)
#
# example:
#   find_package(SenseKit REQUIRED)
#   include_directories(${SENSEKIT_INCLUDE_DIR})
#   add_executable(myapp ...)
#   target_link_libraries(myapp ${SENSEKIT_LIBRARIES})

# define the list of search paths for headers and libraries
set(FIND_SENSEKIT_PATHS
    ${SENSEKIT_ROOT}
    $ENV{SENSEKIT_ROOT}
    ..
    ~/Library/Frameworks
    /Library/Frameworks
    /usr/local
    /usr
    /sw
    /opt/local
    /opt/csw
    /opt)


# find the required libraries
set(SENSEKIT_FOUND TRUE) # will be set to false if one of the required modules is not found

if (NOT SENSEKIT_SDK_BUILD)
    # find the SenseKit include directory
    find_path(SENSEKIT_INCLUDE_DIR SenseKit/SenseKit.h
              PATH_SUFFIXES include
              PATHS ${FIND_SENSEKIT_PATHS})

    set(SENSEKIT_FIND_COMPONENTS
        SenseKit
        SenseKitAPI
        SenseKitUL
        )

    foreach(FIND_SENSEKIT_COMPONENT ${SENSEKIT_FIND_COMPONENTS})
        string(TOUPPER ${FIND_SENSEKIT_COMPONENT} FIND_SENSEKIT_COMPONENT_UPPER)
        
        # dynamic release library
        find_library(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE
                     NAMES ${FIND_SENSEKIT_COMPONENT}
                     PATH_SUFFIXES lib64 lib
                     PATHS ${FIND_SENSEKIT_PATHS})

        # dynamic debug library
        find_library(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG
                     NAMES ${FIND_SENSEKIT_COMPONENT}-d
                     PATH_SUFFIXES lib64 lib
                     PATHS ${FIND_SENSEKIT_PATHS})

        if (SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG OR SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE)
            # library found
            set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_FOUND TRUE)
            
            # if both are found, set SENSEKIT_XXX_LIBRARY to contain both
            if (SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG AND SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE)
                set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY debug     ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG}
                                                              optimized ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE})
            endif()

            # if only one debug/release variant is found, set the other to be equal to the found one
            if (SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG AND NOT SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE)
                # debug and not release
                set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG})
                set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY         ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG})
            endif()
            if (SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE AND NOT SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG)
                # release and not debug
                set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE})
                set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY       ${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE})
            endif()
        else()
            # library not found
            set(SENSEKIT_FOUND FALSE)
            set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_FOUND FALSE)
            set(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY "")
            set(FIND_SENSEKIT_MISSING "${FIND_SENSEKIT_MISSING} SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY")
        endif()

        # mark as advanced
        MARK_AS_ADVANCED(SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY
                         SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_RELEASE
                         SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY_DEBUG)

        # add to the global list of libraries
        set(SENSEKIT_LIBRARIES ${SENSEKIT_LIBRARIES} "${SENSEKIT_${FIND_SENSEKIT_COMPONENT_UPPER}_LIBRARY}")
    endforeach()
endif()

# handle errors
if(NOT SENSEKIT_FOUND)
    # include directory or library not found
    set(FIND_SENSEKIT_ERROR "Could NOT find SenseKit (missing: ${FIND_SENSEKIT_MISSING})")
    if(SENSEKIT_FIND_REQUIRED)
        # fatal error
        message(FATAL_ERROR ${FIND_SENSEKIT_ERROR})
    elseif(NOT SENSEKIT_FIND_QUIETLY)
        # error but continue
        message("${FIND_SENSEKIT_ERROR}")
    endif()
endif()

# handle success
if(SENSEKIT_FOUND)
    message(STATUS "Found SenseKit in ${SENSEKIT_INCLUDE_DIR}")
    message(STATUS "Found SenseKit libraries: ${SENSEKIT_LIBRARIES}")
endif()
