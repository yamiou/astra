#include "TrackingData.h"
#include <queue>
#include "ScalingCoordinateMapper.h"
#include <cmath>
#include "Segmentation.h"
#include "constants.h"
#include <Shiny.h>

#define MAX_DEPTH 10000

namespace sensekit { namespace plugins { namespace hand { namespace segmentation {

    struct PointTTL
    {
        int x;
        int y;
        float ttl;

        PointTTL(int x, int y, float ttl) :
            x(x),
            y(y),
            ttl(ttl)
        { }
    };

    static void enqueue_neighbors(cv::Mat& matVisited,
                                 std::queue<PointTTL>& pointQueue,
                                 PointTTL pt)
    {
        PROFILE_FUNC();
        const int& x = pt.x;
        const int& y = pt.y;
        int width = matVisited.cols;
        int height = matVisited.rows;

        if (x < 1 || x > width - 2 ||
            y < 1 || y > height - 2)
        {
            return;
        }

        float& ttlRef = pt.ttl;

        char& rightVisited = matVisited.at<char>(y, x+1);
        if (0 == rightVisited)
        {
            rightVisited = 1;
            pointQueue.push(PointTTL(x+1, y, ttlRef));
        }

        char& leftVisited = matVisited.at<char>(y, x-1);
        if (0 == leftVisited)
        {
            leftVisited = 1;
            pointQueue.push(PointTTL(x-1, y, ttlRef));
        }

        char& downVisited = matVisited.at<char>(y+1, x);
        if (0 == downVisited)
        {
            downVisited = 1;
            pointQueue.push(PointTTL(x, y+1, ttlRef));
        }

        char& upVisited = matVisited.at<char>(y-1, x);
        if (0 == upVisited)
        {
            upVisited = 1;
            pointQueue.push(PointTTL(x, y-1, ttlRef));
        }
    }

    static cv::Point find_nearest_in_range_pixel(TrackingData& data,
                                                cv::Mat& matVisited)
    {
        PROFILE_FUNC();
        assert(matVisited.size() == data.matrices.depth.size());

        const float minDepth = data.referenceWorldPosition.z - data.bandwidthDepthNear;
        const float maxDepth = data.referenceWorldPosition.z + data.bandwidthDepthFar;
        const float maxSegmentationDist = data.maxSegmentationDist;
        const float referenceAreaSqrt = data.referenceAreaSqrt;
        cv::Mat& depthMatrix = data.matrices.depth;
        cv::Mat& searchedMatrix = data.matrices.foregroundSearched;

        std::queue<PointTTL> pointQueue;

        pointQueue.push(PointTTL(data.seedPosition.x, data.seedPosition.y, maxSegmentationDist));

        matVisited.at<char>(data.seedPosition) = 1;

        while (!pointQueue.empty())
        {
            PointTTL pt = pointQueue.front();
            pointQueue.pop();
            const int& x = pt.x;
            const int& y = pt.y;
            float& ttlRef = pt.ttl;

            if (ttlRef <= 0)
            {
                continue;
            }

            searchedMatrix.at<char>(y, x) = PixelType::SearchedFromOutOfRange;

            float depth = depthMatrix.at<float>(y, x);
            bool pointInRange = depth != 0 && depth > minDepth && depth < maxDepth;

            if (pointInRange)
            {
                return cv::Point(x, y);
            }

            ttlRef -= referenceAreaSqrt;

            enqueue_neighbors(matVisited, pointQueue, pt);
        }

        return INVALID_POINT;
    }

    static float segment_foreground_and_get_average_depth(TrackingData& data)
    {
        PROFILE_FUNC();
        const float& maxSegmentationDist = data.maxSegmentationDist;
        const SegmentationVelocityPolicy& velocitySignalPolicy = data.velocityPolicy;
        const float seedDepth = data.matrices.depth.at<float>(data.seedPosition);
        const float referenceAreaSqrt = data.referenceAreaSqrt;
        cv::Mat& depthMatrix = data.matrices.depth;
        cv::Mat& velocitySignalMatrix = data.matrices.velocitySignal;
        cv::Mat& segmentationMatrix = data.matrices.layerSegmentation;
        cv::Mat& searchedMatrix = data.matrices.foregroundSearched;

        std::queue<PointTTL> pointQueue;

        double totalDepth = 0;
        int depthCount = 0;

        //does the seed point start in range?
        //If not, it will search outward until it finds in range pixels
        const float minDepth = data.referenceWorldPosition.z - data.bandwidthDepthNear;
        const float maxDepth = data.referenceWorldPosition.z + data.bandwidthDepthFar;

        cv::Mat matVisited = cv::Mat::zeros(depthMatrix.size(), CV_8UC1);

        cv::Point seedPosition = data.seedPosition;

        bool seedInRange = seedDepth != 0 && seedDepth > minDepth && seedDepth < maxDepth;
        if (!seedInRange)
        {
            seedPosition = find_nearest_in_range_pixel(data, matVisited);
            if (seedPosition == INVALID_POINT)
            {
                //No in range pixels found, no foreground to set
                return 0.0f;
            }
        }

        pointQueue.push(PointTTL(seedPosition.x, seedPosition.y, maxSegmentationDist));

        matVisited.at<char>(seedPosition) = 1;

        while (!pointQueue.empty())
        {
            PointTTL pt = pointQueue.front();
            pointQueue.pop();
            const int& x = pt.x;
            const int& y = pt.y;
            float& ttlRef = pt.ttl;

            if (velocitySignalPolicy == VELOCITY_POLICY_RESET_TTL &&
                velocitySignalMatrix.at<char>(y, x) == PixelType::Foreground)
            {
                ttlRef = maxSegmentationDist;
            }

            float depth = depthMatrix.at<float>(y, x);
            bool pointOutOfRange = depth == 0 || depth < minDepth || depth > maxDepth;

            if (ttlRef <= 0 || pointOutOfRange)
            {
                continue;
            }

            totalDepth += depth;
            ++depthCount;

            searchedMatrix.at<char>(y, x) = PixelType::Searched;
            segmentationMatrix.at<char>(y, x) = PixelType::Foreground;

            ttlRef -= referenceAreaSqrt;

            enqueue_neighbors(matVisited, pointQueue, pt);
        }

        if (depthCount > 0)
        {
            float averageDepth = static_cast<float>(totalDepth / depthCount);
            return averageDepth;
        }
        else
        {
            return 0.0f;
        }
    }

    void calculate_layer_score(TrackingData& data, const float layerAverageDepth)
    {
        PROFILE_FUNC();
        cv::Mat& edgeDistanceMatrix = data.matrices.layerEdgeDistance;
        const float depthFactor = data.depthFactor;
        const float heightFactor = data.heightFactor;
        const float edgeDistanceFactor = data.edgeDistanceFactor;
        const float targetEdgeDist = data.targetEdgeDistance;
        cv::Mat& layerScoreMatrix = data.matrices.layerScore;
        const float pointInertiaFactor = data.pointInertiaFactor;
        const float pointInertiaRadius = data.pointInertiaRadius;
        const sensekit::Vector3f* worldPoints = data.matrices.worldPoints;
        cv::Mat& segmentationMatrix = data.matrices.layerSegmentation;

        layerScoreMatrix = cv::Mat::zeros(data.matrices.depth.size(), CV_32FC1);

        ScalingCoordinateMapper mapper = get_scaling_mapper(data.matrices);

        auto seedWorldPosition = sensekit::Vector3f(data.referenceWorldPosition.x,
                                                    data.referenceWorldPosition.y,
                                                    data.referenceWorldPosition.z);

        int width = data.matrices.depth.cols;
        int height = data.matrices.depth.rows;

        int edgeRadius = mapper.scale() * width / 32;
        int minX = edgeRadius - 1;
        int maxX = width - edgeRadius;
        int minY = edgeRadius - 1;
        int maxY = height - edgeRadius;

        for (int y = 0; y < height; y++)
        {
            float* edgeDistanceRow = edgeDistanceMatrix.ptr<float>(y);
            float* layerScoreRow = layerScoreMatrix.ptr<float>(y);
            char* segmentationRow = segmentationMatrix.ptr<char>(y);

            for (int x = 0; x < width; ++x,
                                       ++worldPoints,
                                       ++edgeDistanceRow,
                                       ++layerScoreRow,
                                       ++segmentationRow)
            {
                if (*segmentationRow != PixelType::Foreground)
                {
                    continue;
                }

                sensekit::Vector3f worldPosition = *worldPoints;
                if (worldPosition.z != 0 && x > minX && x < maxX && y > minY && y < maxY)
                {
                    //start with arbitrary large value to prevent scores from going negative
                    //(which is ok algorithmically, but debug visuals don't like it)
                    float score = 10000;

                    score += worldPosition.y * heightFactor;

                    float depthDiff = 1.0 - worldPosition.z / layerAverageDepth;
                    score += depthDiff * depthFactor;

                    if (pointInertiaRadius > 0)
                    {
                        auto vector = worldPosition - seedWorldPosition;
                        float length = vector.length();
                        float distFromSeedNorm = std::max(0.0f, std::min(1.0f,
                                                    length / pointInertiaRadius));
                        score += (1.0f - distFromSeedNorm) * pointInertiaFactor;
                    }

                    float edgeDistance = *edgeDistanceRow;
                    float edgeScore = edgeDistanceFactor * std::min(edgeDistance, targetEdgeDist) / targetEdgeDist;

                    score += edgeScore;

                    *layerScoreRow = score;
                }
                else
                {
                    *layerScoreRow = 0;
                }
            }
        }
    }

    static cv::Point track_point_from_seed(TrackingData& data)
    {
        PROFILE_FUNC();
        cv::Size size = data.matrices.depth.size();
        data.matrices.layerSegmentation = cv::Mat::zeros(size, CV_8UC1);
        data.matrices.layerEdgeDistance = cv::Mat::zeros(size, CV_32FC1);
        data.matrices.layerScore = cv::Mat::zeros(size, CV_32FC1);

        const float layerAverageDepth = segment_foreground_and_get_average_depth(data);

        if (layerAverageDepth == 0.0f || data.matrices.layerSegmentation.empty())
        {
            return INVALID_POINT;
        }

        cv::Mat& matScore = data.matrices.layerScore;

        calculate_edge_distance(data.matrices.layerSegmentation,
                                    data.matrices.areaSqrt,
                                    data.matrices.layerEdgeDistance,
                                    data.targetEdgeDistance * 2.0f);
        calculate_layer_score(data, layerAverageDepth);

        double min, max;
        cv::Point minLoc, maxLoc;

        cv::minMaxLoc(matScore, &min, &max, &minLoc, &maxLoc, data.matrices.layerSegmentation);

        if (data.matrices.debugLayersEnabled)
        {
            ++data.matrices.layerCount;

            cv::bitwise_or(cv::Mat(size, CV_8UC1, cv::Scalar(data.matrices.layerCount)),
                data.matrices.debugSegmentation,
                data.matrices.debugSegmentation,
                data.matrices.layerSegmentation);

            cv::Mat scoreMask;
            cv::inRange(matScore, 1, INT_MAX, scoreMask);
            matScore.copyTo(data.matrices.debugScoreValue, scoreMask);
            cv::normalize(matScore, data.matrices.debugScore, 0, 1, cv::NORM_MINMAX, -1, scoreMask);
        }

        if (maxLoc.x == -1 && maxLoc.y == -1)
        {
            return INVALID_POINT;
        }

        return maxLoc;
    }

    cv::Point converge_track_point_from_seed(TrackingData& data)
    {
        PROFILE_FUNC();
        cv::Point point = data.seedPosition;
        cv::Point lastPoint = data.seedPosition;
        int iterations = 0;

        do
        {
            lastPoint = point;
            point = track_point_from_seed(data);
            ++iterations;
        } while (point != lastPoint && iterations < data.iterationMax && point != INVALID_POINT);

        return point;
    }

    bool find_next_velocity_seed_pixel(cv::Mat& velocitySignalMatrix,
                                        cv::Mat& searchedMatrix,
                                        cv::Point& foregroundPosition,
                                        cv::Point& nextSearchStart)
    {
        PROFILE_FUNC();
        assert(velocitySignalMatrix.cols == searchedMatrix.cols);
        assert(velocitySignalMatrix.rows == searchedMatrix.rows);
        int width = velocitySignalMatrix.cols;
        int height = velocitySignalMatrix.rows;

        const int startX = MAX(0, MIN(width - 1, nextSearchStart.x));
        const int startY = MAX(0, MIN(height - 1, nextSearchStart.y));

        for (int y = startY; y < height; y++)
        {
            for (int x = startX; x < width; x++)
            {
                uint8_t velocitySignal = *velocitySignalMatrix.ptr<uint8_t>(y, x);
                uint8_t searched = *searchedMatrix.ptr<uint8_t>(y, x);
                if (velocitySignal == PixelType::Foreground && searched != PixelType::Searched)
                {
                    foregroundPosition.x = x;
                    foregroundPosition.y = y;

                    nextSearchStart.x = x + 1;
                    if (nextSearchStart.x < width)
                    {
                        nextSearchStart.y = y;
                    }
                    else
                    {
                        nextSearchStart.x = 0;
                        nextSearchStart.y = y + 1;
                        if (nextSearchStart.y >= height)
                        {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
        foregroundPosition = segmentation::INVALID_POINT;
        nextSearchStart.x = width;
        nextSearchStart.y = height;
        return false;
    }

    void calculate_edge_distance(cv::Mat& segmentationMatrix,
                                 cv::Mat& areaSqrtMatrix,
                                 cv::Mat& edgeDistanceMatrix,
                                 const float maxEdgeDistance)
    {
        PROFILE_FUNC();
        cv::Mat eroded;
        cv::Mat crossElement = cv::getStructuringElement(cv::MORPH_CROSS, cv::Size(3, 3));

        edgeDistanceMatrix = cv::Mat::zeros(segmentationMatrix.size(), CV_32FC1);
        cv::Mat ones = cv::Mat::ones(segmentationMatrix.size(), CV_32FC1);
        segmentationMatrix.copyTo(eroded);

        //close small holes
        int dilateCount = 1;
        for (int i = 0; i < dilateCount; i++)
        {
            cv::dilate(eroded, eroded, crossElement);
        }

        int nonZeroCount = 0;
        const int imageLength = eroded.cols * eroded.rows;
        int iterations = 0;
        const int maxIterations = segmentationMatrix.cols / 2;
        bool done;
        do
        {
            PROFILE_BEGIN(edge_dist_loop);
            //erode makes the image smaller
            cv::erode(eroded, eroded, crossElement);
            //accumulate the eroded image to the edgeDistance buffer
            cv::add(areaSqrtMatrix, edgeDistanceMatrix, edgeDistanceMatrix, eroded, CV_32FC1);

            nonZeroCount = cv::countNonZero(eroded);
            done = (nonZeroCount == 0);
            double min, max;

            cv::minMaxLoc(edgeDistanceMatrix, &min, &max);
            if (max > maxEdgeDistance)
            {
                done = true;
            }

            PROFILE_END();
            //nonZeroCount < imageLength guards against segmentation with all 1's, which will never erode
        } while (!done && nonZeroCount < imageLength && ++iterations < maxIterations);
    }

    void visit_callback_if_valid_position(int width,
                                          int height,
                                          int x,
                                          int y,
                                          std::function<void(cv::Point)> callback)
    {
        PROFILE_FUNC();
        if (x >= 0 && x < width &&
            y >= 0 && y < height)
        {
            callback(cv::Point(x, y));
        }
    }

    void visit_circle_circumference(cv::Mat& matDepth,
                                    const cv::Point& center,
                                    const float& radius,
                                    const ScalingCoordinateMapper& mapper,
                                    std::function<void(cv::Point)> callback)
    {
        PROFILE_FUNC();
        int width = matDepth.cols;
        int height = matDepth.rows;
        if (center.x < 0 || center.x >= width ||
            center.y < 0 || center.y >= height ||
            radius < 1)
        {
            return;
        }

        float referenceDepth = matDepth.at<float>(center);
        if (referenceDepth == 0)
        {
            return;
        }

        cv::Point offsetRight = offset_pixel_location_by_mm(mapper, center, radius, 0, referenceDepth);

        //http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
        int pixelRadius = offsetRight.x - center.x;
        int cx = center.x;
        int cy = center.y;
        int dx = pixelRadius; //radius in pixels
        int dy = 0;
        int radiusError = 1 - dx;

        while (dx >= dy)
        {
            visit_callback_if_valid_position(width, height, cx + dx, cy + dy, callback);
            visit_callback_if_valid_position(width, height, cx + dy, cy + dx, callback);
            visit_callback_if_valid_position(width, height, cx - dx, cy + dy, callback);
            visit_callback_if_valid_position(width, height, cx - dy, cy + dx, callback);
            visit_callback_if_valid_position(width, height, cx - dx, cy - dy, callback);
            visit_callback_if_valid_position(width, height, cx - dy, cy - dx, callback);
            visit_callback_if_valid_position(width, height, cx + dx, cy - dy, callback);
            visit_callback_if_valid_position(width, height, cx + dy, cy - dx, callback);

            dy++;
            if (radiusError < 0)
            {
                radiusError += 2 * dy + 1;
            }
            else
            {
                dx--;
                radiusError += 2 * (dy - dx) + 1;
            }
        }
    }

    void accumulate_foreground_at_position(cv::Mat& matSegmentation,
                                           cv::Point p,
                                           int& foregroundCount,
                                           int& totalCount)
    {
        PROFILE_FUNC();
        ++totalCount;
        if (matSegmentation.at<uint8_t>(p) == PixelType::Foreground)
        {
            ++foregroundCount;
        }
    }

    float get_percent_foreground_along_circumference(cv::Mat& matDepth,
                                                     cv::Mat& matSegmentation,
                                                     const cv::Point& center,
                                                     const float& radius,
                                                     const ScalingCoordinateMapper& mapper)
    {
        PROFILE_FUNC();
        int foregroundCount = 0;
        int totalCount = 0;

        auto callback = [&](cv::Point p)
        {
            accumulate_foreground_at_position(matSegmentation, p, foregroundCount, totalCount);
        };

        visit_circle_circumference(matDepth, center, radius, mapper, callback);

        float percentForeground = foregroundCount / static_cast<float>(totalCount);

        return percentForeground;
    }

    float count_neighborhood_area(cv::Mat& matSegmentation,
                                  cv::Mat& matDepth,
                                  cv::Mat& matArea,
                                  const cv::Point& center,
                                  const float bandwidth,
                                  const float bandwidthDepth,
                                  const ScalingCoordinateMapper& mapper)
    {
        PROFILE_FUNC();
        if (center.x < 0 || center.y < 0 ||
            center.x >= matDepth.cols || center.y >= matDepth.rows)
        {
            return 0;
        }

        float startingDepth = matDepth.at<float>(center);

        cv::Point topLeft = offset_pixel_location_by_mm(mapper, center, -bandwidth, bandwidth, startingDepth);
        cv::Point bottomRight = offset_pixel_location_by_mm(mapper, center, bandwidth, -bandwidth, startingDepth);

        int32_t x0 = MAX(0, topLeft.x);
        int32_t y0 = MAX(0, topLeft.y);
        int32_t x1 = MIN(matDepth.cols - 1, bottomRight.x);
        int32_t y1 = MIN(matDepth.rows - 1, bottomRight.y);

        float area = 0;

        for (int y = y0; y < y1; y++)
        {
            float* depthRow = matDepth.ptr<float>(y);
            char* segmentationRow = matSegmentation.ptr<char>(y);
            float* areaRow = matArea.ptr<float>(y);

            depthRow += x0;
            segmentationRow += x0;
            areaRow += x0;
            for (int x = x0; x < x1; x++)
            {
                if (*segmentationRow == PixelType::Foreground)
                {
                    float depth = *depthRow;
                    if (std::fabs(depth - startingDepth) < bandwidthDepth)
                    {
                        area += *areaRow;
                    }
                }
                ++depthRow;
                ++areaRow;
                ++segmentationRow;
            }
        }

        return area;
    }

}}}}
