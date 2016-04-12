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
#include "hnd_depth_utility.hpp"
#include <astra/astra.hpp>
#include "hnd_tracking_data.hpp"
#include <cmath>
#include <Shiny.h>

namespace astra { namespace hand {

    depth_utility::depth_utility(float width, float height, depth_utility_settings& settings) :
        processingWidth_(width),
        processingHeight_(height),
        depthSmoothingFactor_(settings.depthSmoothingFactor),
        velocityThresholdFactor_(settings.velocityThresholdFactor),
        maxDepthJumpPercent_(settings.maxDepthJumpPercent),
        erodeSize_(settings.erodeSize),
        depthAdjustmentFactor_(settings.depthAdjustmentFactor),
        minDepth_(settings.minDepth),
        maxDepth_(settings.maxDepth)
    {
        PROFILE_FUNC();
        rectElement_ = cv::getStructuringElement(cv::MORPH_RECT,
                                                 cv::Size(erodeSize_ * 2 + 1, erodeSize_ * 2 + 1),
                                                 cv::Point(erodeSize_, erodeSize_));

        reset();
    }

    depth_utility::~depth_utility()
    {
        PROFILE_FUNC();
    }

    void depth_utility::reset()
    {
        PROFILE_FUNC();
        matDepthFilled_ = cv::Mat::zeros(processingHeight_, processingWidth_, CV_32FC1);
        matDepthFilledMask_ = cv::Mat::zeros(processingHeight_, processingWidth_, CV_8UC1);
        matDepthPrevious_ = cv::Mat::zeros(processingHeight_, processingWidth_, CV_32FC1);
        matDepthAvg_ = cv::Mat::zeros(processingHeight_, processingWidth_, CV_32FC1);
        matDepthVel_.create(processingHeight_, processingWidth_, CV_32FC1);
        matDepthVelErode_.create(processingHeight_, processingWidth_, CV_32FC1);

        for (int i = 0; i < NUM_DEPTH_VEL_CHUNKS; i++)
        {
            maxVel_[i] = 0;
        }
    }

    void depth_utility::depth_to_velocity_signal(const DepthFrame& depthFrame,
                                                 cv::Mat& matDepth,
                                                 cv::Mat& matDepthFullSize,
                                                 cv::Mat& matVelocitySignal)
    {
        PROFILE_FUNC();
        int width = depthFrame.width();
        int height = depthFrame.height();

        depthframe_to_matrix(depthFrame, width, height, matDepthFullSize);

        if (width == processingWidth_ && height == processingHeight_)
        {
            //target size is original size, just use the same data
            matDepth = matDepthFullSize;
        }
        else
        {
            matDepth.create(processingHeight_, processingWidth_, CV_32FC1);
            //convert to the target processing size with nearest neighbor
            cv::resize(matDepthFullSize, matDepth, matDepth.size(), 0, 0, CV_INTER_NN);
        }

        matVelocitySignal = cv::Mat::zeros(processingHeight_, processingWidth_, CV_8UC1);

        //fill 0 depth pixels with the value from the previous frame
        fill_zero_values(matDepth, matDepthFilled_, matDepthFilledMask_, matDepthPrevious_);

        //accumulate current frame to average using smoothing factor

        cv::accumulateWeighted(matDepthFilled_, matDepthAvg_, depthSmoothingFactor_);

        filter_zero_values_and_jumps(matDepthFilled_,
                                     matDepthPrevious_,
                                     matDepthAvg_,
                                     matDepthFilledMask_,
                                     maxDepthJumpPercent_);

        //current minus average, scaled by average = velocity as a percent change

        matDepthVel_ = (matDepthFilled_ - matDepthAvg_) / matDepthAvg_;

        adjust_velocities_for_depth(matDepth, matDepthVel_);

        //erode to eliminate single pixel velocity artifacts
        matDepthVelErode_ = cv::abs(matDepthVel_);
        cv::erode(matDepthVelErode_, matDepthVelErode_, rectElement_);
        //cv::dilate(matDepthVelErode_, matDepthVelErode_, rectElement_);

        threshold_velocity_signal(matDepthVelErode_,
                                  matVelocitySignal,
                                  velocityThresholdFactor_);

        //analyze_velocities(matDepth, matDepthVelErode_);
    }

    void depth_utility::depthframe_to_matrix(const DepthFrame& depthFrameSrc,
                                             const int width,
                                             const int height,
                                             cv::Mat& matTarget)
    {
        PROFILE_FUNC();
        //ensure initialized
        matTarget.create(height, width, CV_32FC1);

        const int16_t* depthData = depthFrameSrc.data();

        for (int y = 0; y < height; ++y)
        {
            float* row = matTarget.ptr<float>(y);
            for (int x = 0; x < width; ++x)
            {
                float depth = static_cast<float>(*depthData);
                *row = depth;
                ++row;
                ++depthData;
            }
        }
    }

    void depth_utility::fill_zero_values(cv::Mat& matDepth,
                                         cv::Mat& matDepthFilled,
                                         cv::Mat& matDepthFilledMask,
                                         cv::Mat& matDepthPrevious)
    {
        PROFILE_FUNC();
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* filledDepthRow = matDepthFilled.ptr<float>(y);
            uint8_t* filledDepthMaskRow = matDepthFilledMask.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x)
            {
                float depth = *depthRow;

                if (depth == 0)
                {
                    depth = *prevDepthRow;
                    *filledDepthMaskRow = static_cast<uint8_t>(fill_mask_type::filled);
                }
                else
                {
                    *filledDepthMaskRow = static_cast<uint8_t>(fill_mask_type::normal);
                }

                *filledDepthRow = depth;

                ++depthRow;
                ++prevDepthRow;
                ++filledDepthRow;
                ++filledDepthMaskRow;
            }
        }
    }

    void depth_utility::filter_zero_values_and_jumps(cv::Mat& matDepth,
                                                     cv::Mat& matDepthPrevious,
                                                     cv::Mat& matDepthAvg,
                                                     cv::Mat& matDepthFilledMask,
                                                     const float maxDepthJumpPercent)
    {
        PROFILE_FUNC();
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* prevDepthRow = matDepthPrevious.ptr<float>(y);
            float* avgRow = matDepthAvg.ptr<float>(y);
            uint8_t* filledDepthMaskRow = matDepthFilledMask.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x, ++depthRow, ++prevDepthRow, ++filledDepthMaskRow)
            {
                float depth = *depthRow;
                float previousDepth = *prevDepthRow;
                fill_mask_type fillType = static_cast<fill_mask_type>(*filledDepthMaskRow);

                //calculate percent change since last frame
                float deltaPercent = (depth - previousDepth) / previousDepth;
                float absDeltaPercent = std::fabs(deltaPercent);
                bool movingAway = deltaPercent > 0;

                //suppress signal if either current or previous pixel are invalid
                bool isZeroDepth = (0 == depth || 0 == previousDepth);

                //suppress signal when a pixel was artificially filled
                bool isFilled = (fillType == fill_mask_type::filled);

                //suppress signal when a pixel jumps a long distance from near to far
                bool isJumpingAway = (absDeltaPercent > maxDepthJumpPercent && movingAway);

                if (isZeroDepth || isFilled || isJumpingAway)
                {
                    //set the average to the current depth, and set velocity to zero
                    //this suppresses the velocity signal for edge jumping artifacts
                    avgRow[x] = depth;
                }

                *prevDepthRow = depth;
            }
        }
    }

    void depth_utility::threshold_velocity_signal(cv::Mat& matVelocityFiltered,
                                                  cv::Mat& matVelocitySignal,
                                                  const float velocityThresholdFactor)
    {
        PROFILE_FUNC();
        int width = matVelocitySignal.cols;
        int height = matVelocitySignal.rows;

        for (int y = 0; y < height; ++y)
        {
            float* velFilteredRow = matVelocityFiltered.ptr<float>(y);
            uint8_t* velocitySignalRow = matVelocitySignal.ptr<uint8_t>(y);

            for (int x = 0; x < width; ++x, ++velFilteredRow, ++velocitySignalRow)
            {
                //matVelocityFiltered is already abs(vel)
                float velFiltered = *velFilteredRow;

                if (velFiltered > velocityThresholdFactor)
                {
                    *velocitySignalRow = pixel_type::foreground;
                }
                else
                {
                    *velocitySignalRow = pixel_type::background;
                }
            }
        }
    }

    void depth_utility::adjust_velocities_for_depth(cv::Mat& matDepth, cv::Mat& matVelocityFiltered)
    {
        PROFILE_FUNC();
        if (depthAdjustmentFactor_ == 0)
        {
            return;
        }

        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* velFilteredRow = matVelocityFiltered.ptr<float>(y);

            for (int x = 0; x < width; ++x, ++depthRow, ++velFilteredRow)
            {
                float depth = *depthRow;
                if (depth != 0.0f)
                {
                    float& velFiltered = *velFilteredRow;
                    if (depth > minDepth_ && depth < maxDepth_)
                    {
                        float depthM = depth / 1000.0f;
                        velFiltered /= depthM * depthAdjustmentFactor_;
                    }
                    else
                    {
                        velFiltered = 0;
                    }
                }
            }
        }
    }

    int depth_utility::depth_to_chunk_index(float depth)
    {
        PROFILE_FUNC();
        if (depth == 0 || depth < MIN_CHUNK_DEPTH || depth > MAX_CHUNK_DEPTH)
        {
            return -1;
        }
        const float chunkRange = MAX_CHUNK_DEPTH - MIN_CHUNK_DEPTH;
        const float normDepth = (depth - MIN_CHUNK_DEPTH) / chunkRange;

        return std::min(NUM_DEPTH_VEL_CHUNKS - 1, static_cast<int>(NUM_DEPTH_VEL_CHUNKS * normDepth));
    }

    void depth_utility::analyze_velocities(cv::Mat& matDepth, cv::Mat& matVelocityFiltered)
    {
        PROFILE_FUNC();
        int width = matDepth.cols;
        int height = matDepth.rows;

        for (int i = 0; i < NUM_DEPTH_VEL_CHUNKS; i++)
        {
            maxVel_[i] *= velErodeFactor_;
            depthCount_[i] = 0;
        }

        for (int y = 0; y < height; ++y)
        {
            float* depthRow = matDepth.ptr<float>(y);
            float* velFilteredRow = matVelocityFiltered.ptr<float>(y);

            for (int x = 0; x < width; ++x, ++depthRow, ++velFilteredRow)
            {
                //matVelocityFiltered is already abs(vel)

                float depth = *depthRow;
                int chunkIndex = depth_to_chunk_index(depth);
                if (chunkIndex >= 0 && depth != 0)
                {
                    ++depthCount_[chunkIndex];

                    float& maxVel = maxVel_[chunkIndex];
                    float velFiltered = *velFilteredRow;

                    if (velFiltered > maxVel)
                    {
                        maxVel = velFiltered;
                    }
                }
            }
        }

        const float chunkRange = MAX_CHUNK_DEPTH - MIN_CHUNK_DEPTH;
        const float chunk_size = chunkRange / NUM_DEPTH_VEL_CHUNKS;

        for (int i = 0; i < NUM_DEPTH_VEL_CHUNKS; i++)
        {
            float maxVel = maxVel_[i];
            int count = depthCount_[i];
            float startDepth = (MIN_CHUNK_DEPTH + i * chunk_size) / 1000.0f;
            float endDepth = (MIN_CHUNK_DEPTH + (1 + i) * chunk_size) / 1000.0f;
            float ratio = 0;
            if (endDepth != 0.0f)
            {
                ratio = maxVel / endDepth;
            }

            printf("[%.1fm %d,%f,%f] ", startDepth, count, maxVel, ratio);
            if (i % 3 == 2)
            {
                printf("\n");
            }
        }
        printf("[%.1fm]\n\n", static_cast<int>(MAX_CHUNK_DEPTH)/1000.0f);
    }
}}
