// This file is part of the Orbbec Astra SDK [https://orbbec3d.com]
// Copyright (c) 2015 Orbbec 3D
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Be excellent to each other.
#ifndef HND_TRACKED_POINT_H
#define HND_TRACKED_POINT_H

#include <opencv2/core/affine.hpp>
#include "hnd_tracking_data.hpp"

namespace astra { namespace hand {

    struct tracked_point
    {
    public:
        cv::Point position;
        cv::Point3f worldPosition;
        cv::Point3f worldDeltaPosition;
        cv::Point fullSizePosition;
        cv::Point3f fullSizeWorldPosition;
        cv::Point3f fullSizeWorldDeltaPosition;
        cv::Point3f steadyWorldPosition;
        int trackingId;
        int inactiveFrameCount;
        int failedTestCount;
        bool isInProbation;
        int probationFrameCount;
        tracked_point_type pointType;
        tracking_status trackingStatus;
        float referenceAreaSqrt;

        tracked_point(cv::Point position, cv::Point3f worldPosition, int trackingId) :
            position(position),
            worldPosition(worldPosition),
            worldDeltaPosition(),
            fullSizePosition(),
            fullSizeWorldPosition(),
            fullSizeWorldDeltaPosition(),
            steadyWorldPosition(worldPosition),
            trackingId(trackingId),
            inactiveFrameCount(0),
            failedTestCount(0),
            isInProbation(true),
            probationFrameCount(0),
            pointType(tracked_point_type::candidate_point),
            trackingStatus(tracking_status::not_tracking),
            referenceAreaSqrt(0)
        { }
    };
}}

#endif // HND_TRACKED_POINT_H
