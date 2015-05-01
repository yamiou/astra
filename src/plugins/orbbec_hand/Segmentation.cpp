#include "TrackingData.h"
#include <queue>
#include "ScalingCoordinateMapper.h"
#include <cmath>
#include <opencv2/opencv.hpp>
#include "Segmentation.h"

#define MAX_DEPTH 10000

namespace sensekit { namespace plugins { namespace hand { namespace segmentation {

    struct PointTTL
    {
        cv::Point point;
        float ttl;

        PointTTL(cv::Point point, float ttl) :
            point(point),
            ttl(ttl)
        { }
    };

    static cv::Point INVALID_POINT(-1, -1);

    static void enqueue_neighbors(cv::Mat& matVisited,
                                 std::queue<PointTTL>& pointQueue,
                                 PointTTL pt)
    {
        const cv::Point& p = pt.point;
        float& ttlRef = pt.ttl;

        int width = matVisited.cols;
        int height = matVisited.rows;

        cv::Point right(p.x + 1, p.y);
        cv::Point left(p.x - 1, p.y);
        cv::Point down(p.x, p.y + 1);
        cv::Point up(p.x, p.y - 1);

        if (right.x < width && 0 == matVisited.at<char>(right))
        {
            matVisited.at<char>(right) = 1;
            pointQueue.push(PointTTL(right, ttlRef));
        }
        if (left.x >= 0 && 0 == matVisited.at<char>(left))
        {
            matVisited.at<char>(left) = 1;
            pointQueue.push(PointTTL(left, ttlRef));
        }
        if (down.y < height && 0 == matVisited.at<char>(down))
        {
            matVisited.at<char>(down) = 1;
            pointQueue.push(PointTTL(down, ttlRef));
        }
        if (up.y >= 0 && 0 == matVisited.at<char>(up))
        {
            matVisited.at<char>(up) = 1;
            pointQueue.push(PointTTL(up, ttlRef));
        }
    }

    static cv::Point find_nearest_in_range_pixel(TrackingData& data,
                                                cv::Mat& matVisited)
    {
        assert(matVisited.size() == data.matrices.depth.size());

        const float maxDepth = data.referenceDepth + data.bandwidthDepth;
        const float maxSegmentationDist = data.maxSegmentationDist;
        cv::Mat& depthMatrix = data.matrices.depth;
        cv::Mat& searchedMatrix = data.matrices.foregroundSearched;
        bool isCandidatePoint = data.pointType == TrackedPointType::CandidatePoint;
        cv::Mat& areaSqrtMatrix = data.matrices.areaSqrt;

        std::queue<PointTTL> pointQueue;

        pointQueue.push(PointTTL(data.seedPosition, maxSegmentationDist));

        matVisited.at<char>(data.seedPosition) = 1;

        while (!pointQueue.empty())
        {
            PointTTL pt = pointQueue.front();
            pointQueue.pop();
            const cv::Point& p = pt.point;
            float& ttlRef = pt.ttl;

            if (ttlRef <= 0)
            {
                continue;
            }

            searchedMatrix.at<char>(p) = PixelType::Searched;

            float depth = depthMatrix.at<float>(p);
            bool pointInRange = depth != 0 && depth < maxDepth;

            if (pointInRange)
            {
                return p;
            }

            if (isCandidatePoint)
            {
                //active points get infinite recovery distance when the seed is out of range
                ttlRef -= areaSqrtMatrix.at<float>(p);
            }

            enqueue_neighbors(matVisited, pointQueue, pt);
        }

        return INVALID_POINT;
    }

    static void segment_foreground(TrackingData& data)
    {
        const float& maxSegmentationDist = data.maxSegmentationDist;
        const SegmentationVelocityPolicy& velocitySignalPolicy = data.velocityPolicy;
        const float seedDepth = data.matrices.depth.at<float>(data.seedPosition);
        cv::Mat& depthMatrix = data.matrices.depth;
        cv::Mat& velocitySignalMatrix = data.matrices.velocitySignal;
        cv::Mat& areaSqrtMatrix = data.matrices.areaSqrt;
        cv::Mat& segmentationMatrix = data.matrices.layerSegmentation;
        cv::Mat& searchedMatrix = data.matrices.foregroundSearched;

        std::queue<PointTTL> pointQueue;

        //does the seed point start in range?
        //If not, it will search outward until it finds in range pixels
        const float maxDepth = data.referenceDepth + data.bandwidthDepth;
            
        cv::Mat matVisited = cv::Mat::zeros(depthMatrix.size(), CV_8UC1);

        cv::Point seedPosition = data.seedPosition;
            
        bool seedInRange = seedDepth != 0 && seedDepth < maxDepth;
        if (!seedInRange)
        {
            seedPosition = find_nearest_in_range_pixel(data, matVisited);
            if (seedPosition == INVALID_POINT)
            {
                //No in range pixels found, no foreground to set
                return;
            }
        }

        pointQueue.push(PointTTL(seedPosition, maxSegmentationDist));

        matVisited.at<char>(data.seedPosition) = 1;

        while (!pointQueue.empty())
        {
            PointTTL pt = pointQueue.front();
            pointQueue.pop();
            const cv::Point& p = pt.point;
            float& ttlRef = pt.ttl;

            if (velocitySignalPolicy == VELOCITY_POLICY_RESET_TTL &&
                velocitySignalMatrix.at<char>(p) == PixelType::Foreground)
            {
                ttlRef = maxSegmentationDist;
            }

            float depth = depthMatrix.at<float>(p);
            bool pointOutOfRange = depth == 0 || depth > maxDepth;

            if (ttlRef <= 0 || pointOutOfRange)
            {
                continue;
            }

            searchedMatrix.at<char>(p) = PixelType::Searched;
            segmentationMatrix.at<char>(p) = PixelType::Foreground;

            ttlRef -= areaSqrtMatrix.at<float>(p);

            enqueue_neighbors(matVisited, pointQueue, pt);
        }
    }

    void calculate_layer_score(TrackingData& data)
    {
        cv::Mat& depthMatrix = data.matrices.depth;
        cv::Mat& basicScoreMatrix = data.matrices.basicScore;
        cv::Mat& edgeDistanceMatrix = data.matrices.layerEdgeDistance;
        const float edgeDistanceFactor = data.edgeDistanceFactor;
        const float targetEdgeDist = data.targetEdgeDistance;
        cv::Mat& layerScoreMatrix = data.matrices.layerScore;
        const float pointInertiaFactor = data.pointInertiaFactor;
        const float pointInertiaRadius = data.pointInertiaRadius;

        ScalingCoordinateMapper mapper = get_scaling_mapper(data.matrices);

        cv::Point3f seedWorldPosition = mapper.convert_depth_to_world(data.seedPosition.x,
                                                                        data.seedPosition.y,
                                                                        data.referenceDepth);
        bool activePoint = data.pointType == TrackedPointType::ActivePoint;

        int width = depthMatrix.cols;
        int height = depthMatrix.rows;

        for (int y = 0; y < height; y++)
        {
            float* depthRow = depthMatrix.ptr<float>(y);
            float* basicScoreRow = basicScoreMatrix.ptr<float>(y);
            float* edgeDistanceRow = edgeDistanceMatrix.ptr<float>(y);
            float* layerScoreRow = layerScoreMatrix.ptr<float>(y);

            for (int x = 0; x < width; x++)
            {
                float depth = *depthRow;
                if (depth != 0)
                {
                    cv::Point3f worldPosition = mapper.convert_depth_to_world(x, y, depth);

                    float score = *basicScoreRow;

                    if (activePoint && pointInertiaRadius > 0)
                    {
                        float distFromSeedNorm = std::max(0.0, std::min(1.0, 
                                                    cv::norm(worldPosition - seedWorldPosition) / pointInertiaRadius));
                        score += (1.0f - distFromSeedNorm) * pointInertiaFactor;
                    }
                        
                    float edgeDistance = *edgeDistanceRow;
                    float edgeScore = (targetEdgeDist - abs(targetEdgeDist - edgeDistance)) * edgeDistanceFactor;
                    if (edgeScore > 0)
                    {
                        score += edgeScore;
                    }
                    else
                    {
                        score = 0;
                    }


                    *layerScoreRow = score;
                }
                else
                {
                    *layerScoreRow = 0;
                }
                ++depthRow;
                ++basicScoreRow;
                ++edgeDistanceRow;
                ++layerScoreRow;
            }
        }
    }

    static cv::Point track_point_from_seed(TrackingData& data)
    {
        cv::Size size = data.matrices.depth.size();
        data.matrices.layerSegmentation = cv::Mat::zeros(size, CV_8UC1);
        data.matrices.layerEdgeDistance = cv::Mat::zeros(size, CV_32FC1);
        data.matrices.layerScore = cv::Mat::zeros(size, CV_32FC1);
        //data.matrices.matLayerSegmentation.setTo(cv::Scalar(0));

        segment_foreground(data);
            
        calculate_edge_distance(data.matrices.layerSegmentation,
                                data.matrices.areaSqrt,
                                data.matrices.layerEdgeDistance);

        calculate_layer_score(data);

        double min, max;
        cv::Point minLoc, maxLoc;

        cv::minMaxLoc(data.matrices.layerScore, &min, &max, &minLoc, &maxLoc, data.matrices.layerSegmentation);

        if (data.matrices.debugLayersEnabled)
        {
            ++data.matrices.layerCount;

            cv::bitwise_or(cv::Mat(size, CV_8UC1, cv::Scalar(data.matrices.layerCount)),
                data.matrices.debugSegmentation,
                data.matrices.debugSegmentation,
                data.matrices.layerSegmentation);
            if (data.pointType == TrackedPointType::ActivePoint)
            {
                cv::Mat scoreMask;
                    
                cv::inRange(data.matrices.layerScore, 1, INT_MAX, scoreMask);
                cv::normalize(data.matrices.layerScore, data.matrices.debugScore, 0, 1, cv::NORM_MINMAX, -1, scoreMask);
            }
        }

        return maxLoc;
    }

    cv::Point converge_track_point_from_seed(TrackingData& data)
    {
        cv::Point point = data.seedPosition;
        cv::Point lastPoint = data.seedPosition;
        int iterations = 0;

        do
        {
            lastPoint = point;
            point = track_point_from_seed(data);
            ++iterations;
        } while (point != lastPoint && iterations < data.iterationMax && point.x != -1 && point.y != -1);

        return point;
    }

    bool find_next_velocity_seed_pixel(cv::Mat& velocitySignalMatrix,
                                        cv::Mat& searchedMatrix,
                                        cv::Point& foregroundPosition,
                                        cv::Point& nextSearchStart)
    {
        assert(foregroundMatrix.cols == searchedMatrix.cols);
        assert(foregroundMatrix.rows == searchedMatrix.rows);
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
        foregroundPosition.x = -1;
        foregroundPosition.y = -1;
        nextSearchStart.x = width;
        nextSearchStart.y = height;
        return false;
    }

    void calculate_basic_score(cv::Mat& depthMatrix,
                                cv::Mat& scoreMatrix,
                                const float heightFactor,
                                const float depthFactor,
                                const ScalingCoordinateMapper& mapper)
    {
        scoreMatrix = cv::Mat::zeros(depthMatrix.size(), CV_32FC1);

        int width = depthMatrix.cols;
        int height = depthMatrix.rows;

        for (int y = 0; y < height; y++)
        {
            float* depthRow = depthMatrix.ptr<float>(y);
            float* scoreRow = scoreMatrix.ptr<float>(y);

            for (int x = 0; x < width; x++)
            {
                float depth = *depthRow;
                if (depth != 0)
                {
                    cv::Point3f worldPosition = mapper.convert_depth_to_world(x, y, depth);

                    float score = 0;
                    score += worldPosition.y * heightFactor;
                    score += (MAX_DEPTH - worldPosition.z) * depthFactor;

                    *scoreRow = score;
                }
                else
                {
                    *scoreRow = 0;
                }
                ++depthRow;
                ++scoreRow;
            }
        }
    }

    void calculate_edge_distance(cv::Mat& segmentationMatrix,
                                    cv::Mat& areaSqrtMatrix,
                                    cv::Mat& edgeDistanceMatrix)
    {
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
            //erode makes the image smaller
            cv::erode(eroded, eroded, crossElement);
            //accumulate the eroded image to the edgeDistance buffer
            cv::add(areaSqrtMatrix, edgeDistanceMatrix, edgeDistanceMatrix, eroded, CV_32FC1);

            nonZeroCount = cv::countNonZero(eroded);
            done = (nonZeroCount == 0);

            //nonZeroCount < imageLength guards against segmentation with all 1's, which will never erode
        } while (!done && nonZeroCount < imageLength && ++iterations < maxIterations);
    }

    static float get_depth_area(cv::Point3f& p1,
                                cv::Point3f& p2,
                                cv::Point3f& p3,
                                const ScalingCoordinateMapper& mapper)
    {
        cv::Point3f world1 = mapper.convert_depth_to_world(p1);
        cv::Point3f world2 = mapper.convert_depth_to_world(p2);
        cv::Point3f world3 = mapper.convert_depth_to_world(p3);

        cv::Point3f v1 = world2 - world1;
        cv::Point3f v2 = world3 - world1;

        float area = static_cast<float>(0.5 * cv::norm(v1.cross(v2)));
        return area;
    }

    void calculate_segment_area(cv::Mat& depthMatrix, cv::Mat& areaMatrix, cv::Mat& areaSqrtMatrix, const ScalingCoordinateMapper& mapper)
    {
        int width = depthMatrix.cols;
        int height = depthMatrix.rows;

        areaMatrix = cv::Mat::zeros(depthMatrix.size(), CV_32FC1);
        areaSqrtMatrix = cv::Mat::zeros(depthMatrix.size(), CV_32FC1);

        for (int y = 0; y < height - 1; y++)
        {
            float* depthRow = depthMatrix.ptr<float>(y);
            float* nextDepthRow = depthMatrix.ptr<float>(y + 1);
            float* areaRow = areaMatrix.ptr<float>(y);
            float* areaSqrtRow = areaSqrtMatrix.ptr<float>(y);

            for (int x = 0; x < width - 1; x++)
            {
                float area = 0;
                float depth1 = *depthRow;

                if (depth1 != 0)
                {
                    cv::Point3f p1(x, y, depth1);
                    cv::Point3f p2(x + 1, y, depth1);
                    cv::Point3f p3(x, y + 1, depth1);
                    cv::Point3f p4(x + 1, y + 1, depth1);

                    area += get_depth_area(p1, p2, p3, mapper);
                    area += get_depth_area(p2, p3, p4, mapper);
                }

                *areaRow = area;
                *areaSqrtRow = sqrt(area);

                ++depthRow;
                ++nextDepthRow;
                ++areaRow;
                ++areaSqrtRow;
            }
        }
    }


    float count_neighborhood_area(cv::Mat& segmentationMatrix,
                                    cv::Mat& depthMatrix,
                                    cv::Mat& areaMatrix,
                                    const cv::Point& center,
                                    const float bandwidth,
                                    const float bandwidthDepth,
                                    const ScalingCoordinateMapper& mapper)
    {
        if (center.x < 0 || center.y < 0 ||
            center.x >= depthMatrix.cols || center.y >= depthMatrix.rows)
        {
            return 0;
        }

        float startingDepth = depthMatrix.at<float>(center);

        cv::Point topLeft = offset_pixel_location_by_mm(mapper, center, -bandwidth, bandwidth, startingDepth);
        cv::Point bottomRight = offset_pixel_location_by_mm(mapper, center, bandwidth, -bandwidth, startingDepth);

        int32_t x0 = MAX(0, topLeft.x);
        int32_t y0 = MAX(0, topLeft.y);
        int32_t x1 = MIN(depthMatrix.cols - 1, bottomRight.x);
        int32_t y1 = MIN(depthMatrix.rows - 1, bottomRight.y);

        float area = 0;

        for (int y = y0; y < y1; y++)
        {
            float* depthRow = depthMatrix.ptr<float>(y);
            char* segmentationRow = segmentationMatrix.ptr<char>(y);
            float* areaRow = areaMatrix.ptr<float>(y);

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
