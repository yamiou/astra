#include "TrajectoryAnalyzer.h"
#include "constants.h"
#include <SenseKit/Plugins/PluginLogger.h>

namespace sensekit { namespace plugins { namespace hand {

    using namespace std;


    TrajectoryAnalyzer::TrajectoryAnalyzer(int trackingId, PluginLogger& pluginLogger, HandSettings& settings) :
        m_trackingId(trackingId),
        m_logger(pluginLogger),
        m_pointSteady(false),
        m_numSteadyFrames(0),
        m_accumulatedDeltaHeading(),
        m_lastAccumulatedDeltaHeading(),
        m_avgDeltaHeadingValid(false),
        m_lastAvgDeltaHeadingValid(false),
        m_isTrackingHeading(false),
        m_headingTrackStart(),
        m_isInflecting(false),
        m_numWaveInflections(0),
        m_isWaveGesture(false),
        m_framesSinceInflection(0),
        /* settings below */
        m_maxSteadyDelta(5),
        m_minSteadyFrames(15),
        m_minHeadingDist(75),
        m_deltaHeadingFactor(0.25),
        m_minHeadingDiffForInflection(135),
        m_maxHeadingDiffForContinuation(45),
        m_minWaveInflectionsForGesture(2),
        m_maxFramesBetweenInflections(90)
    {
    }

    TrajectoryAnalyzer::~TrajectoryAnalyzer()
    {
    }

    void TrajectoryAnalyzer::reset_steady()
    {
        m_pointSteady = false;
        m_numSteadyFrames = 0;
    }

    void TrajectoryAnalyzer::reset_wave()
    {
        m_logger.info("Reset wave gesture for point #%d", m_trackingId);
        
        m_isWaveGesture = false;
        m_isInflecting = false;
        m_lastAccumulatedDeltaHeading = cv::Point3f();
        m_lastAvgDeltaHeadingValid = false;
        m_avgDeltaHeadingValid = false;
        m_isTrackingHeading = false;
        m_numWaveInflections = 0;
    }

    void TrajectoryAnalyzer::update(TrackedPoint& point)
    {
        if (point.trackingId != m_trackingId)
        {
            throw std::logic_error("TrajectoryAnalyzer updated with wrong tracking id");
        }
        ++m_framesSinceInflection;
        if (m_framesSinceInflection == m_maxFramesBetweenInflections)
        {
            m_logger.info("Wave gesture timed out for point #%d", m_trackingId);

            reset_wave();
        }

        cv::Point3f deltaPosition = point.fullSizeWorldDeltaPosition;
        
        float delta = cv::norm(deltaPosition);
        
        if (delta > m_maxSteadyDelta)
        {
            cv::Point3f deltaPositionNullY = deltaPosition;
            deltaPositionNullY.y = 0;
            
            if (!m_isTrackingHeading)
            {
                m_isTrackingHeading = true;
                m_headingTrackStart = point.fullSizeWorldPosition;
                m_accumulatedDeltaHeading = deltaPositionNullY;
            }
            else
            {
                m_accumulatedDeltaHeading += deltaPositionNullY;

                

                m_avgDeltaHeadingValid = is_valid_heading_dist(point.fullSizeWorldPosition);
                
                float headingDist = cv::norm(point.fullSizeWorldPosition - m_headingTrackStart);

                m_logger.trace("Point #%d dist %f v1: %d v2: %d", m_trackingId, headingDist, m_avgDeltaHeadingValid, m_lastAvgDeltaHeadingValid);

                if (m_avgDeltaHeadingValid && m_lastAvgDeltaHeadingValid)
                {
                    float degreeDifference = get_degree_difference(m_accumulatedDeltaHeading, m_lastAccumulatedDeltaHeading);

                    if (degreeDifference > m_minHeadingDiffForInflection)
                    {
                        if (!m_isInflecting)
                        {
                            m_isInflecting = true;
                            m_framesSinceInflection = 0;
                            ++m_numWaveInflections;
                            if (!m_isWaveGesture)
                            {
                                m_logger.info("Wave count %d for point #%d", m_numWaveInflections, m_trackingId);
                                if (m_numWaveInflections == m_minWaveInflectionsForGesture)
                                {
                                    m_logger.info("Wave gesture detected for point #%d", m_trackingId);
                                    m_isWaveGesture = true;
                                }
                            }
                        }
                    }
                    else if (degreeDifference < m_maxHeadingDiffForContinuation)
                    {
                        m_isInflecting = false;
                    }
                    else
                    {
                        reset_wave();
                    }
                }
            }
        }
        else if (m_avgDeltaHeadingValid)
        {
            m_lastAccumulatedDeltaHeading = m_accumulatedDeltaHeading;
            m_isInflecting = false;
            m_avgDeltaHeadingValid = false;
            m_lastAvgDeltaHeadingValid = true;
            m_isTrackingHeading = false;
        }

        m_worldDelta *= .98;
        if (delta > m_worldDelta)
        {
            m_worldDelta = delta;
        }
        
        if (delta < m_maxSteadyDelta)
        {
            ++m_numSteadyFrames;
            if (!m_pointSteady && m_numSteadyFrames > m_minSteadyFrames)
            {
                m_pointSteady = true;

                m_logger.info("Steady gesture detected for point #%d", m_trackingId);

                if (m_isWaveGesture)
                {
                    reset_wave();
                }
            }
        }
        else
        {
            m_numSteadyFrames = 0;
            m_pointSteady = false;
        }
    }

    float TrajectoryAnalyzer::get_delta_angle(float x, float y)
    {
        float radians = std::atan2(y, x);
        float degrees = radians * RAD_TO_DEG;
        return degrees;
    }

    bool TrajectoryAnalyzer::is_valid_heading_dist(const cv::Point3f& currentWorldPosition)
    {
        float headingDist = cv::norm(currentWorldPosition - m_headingTrackStart);
        bool validDist = headingDist > m_minHeadingDist;
        return validDist;
    }

    float TrajectoryAnalyzer::get_degree_difference(cv::Point3f& v1, cv::Point3f& v2)
    {
        float len1 = cv::norm(v1);
        float len2 = cv::norm(v2);
        if (len1 < EPSILON || len2 < EPSILON)
        {
            return 0;
        }

        float invLen1 = 1.0 / len1;
        float invLen2 = 1.0 / len2;
        
        auto norm1 = v1 * invLen1;
        auto norm2 = v2 * invLen2;
        
        float angleBetween = std::acos(norm1.dot(norm2));
        float degreeBetween = angleBetween * RAD_TO_DEG;
        return degreeBetween;
    }
}}}
