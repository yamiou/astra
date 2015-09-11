#include "TrajectoryAnalyzer.h"
#include "constants.h"
#include <Astra/Plugins/PluginLogger.h>

namespace astra { namespace plugins { namespace hand {

    using namespace std;


    TrajectoryAnalyzer::TrajectoryAnalyzer(int trackingId, TrajectoryAnalyzerSettings& settings) :
        m_trackingId(trackingId),
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
        m_recentDeltaHeading(),
        /* settings below */
        m_maxSteadyDelta(settings.maxSteadyDelta),
        m_minSteadyFrames(settings.minSteadyFrames),
        m_minHeadingDist(settings.minHeadingDist),
        m_deltaHeadingFactor(settings.deltaHeadingFactor),
        m_minHeadingDiffForInflection(settings.minHeadingDiffForInflection),
        m_maxHeadingDiffForContinuation(settings.maxHeadingDiffForContinuation),
        m_minWaveInflectionsForGesture(settings.minWaveInflectionsForGesture),
        m_maxFramesBetweenInflections(settings.maxFramesBetweenInflections)
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
        LOG_INFO("TrajectoryAnalyzer", "Reset wave gesture for point #%d", m_trackingId);

        m_isWaveGesture = false;
        m_isInflecting = false;
        m_lastAccumulatedDeltaHeading = cv::Point3f();
        m_lastAvgDeltaHeadingValid = false;
        m_avgDeltaHeadingValid = false;
        m_isTrackingHeading = false;
        m_numWaveInflections = 0;
        m_recentDeltaHeading = cv::Point3f();
    }

    void TrajectoryAnalyzer::set_for_next_inflection()
    {
        m_lastAccumulatedDeltaHeading = m_accumulatedDeltaHeading;
        m_isInflecting = false;
        m_avgDeltaHeadingValid = false;
        m_lastAvgDeltaHeadingValid = true;
        m_isTrackingHeading = false;
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
            LOG_INFO("TrajectoryAnalyzer", "Wave gesture timed out for point #%d", m_trackingId);

            reset_wave();
        }

        cv::Point3f deltaPosition = point.fullSizeWorldDeltaPosition;

        float delta = static_cast<float>(cv::norm(deltaPosition));

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

                m_recentDeltaHeading = deltaPositionNullY * (1 - m_deltaHeadingFactor) + deltaPositionNullY * m_deltaHeadingFactor;

                m_avgDeltaHeadingValid = is_valid_heading_dist(point.fullSizeWorldPosition);

                float headingDist = static_cast<float>(cv::norm(point.fullSizeWorldPosition - m_headingTrackStart));

                LOG_TRACE("TrajectoryAnalyzer", "#%d dist %f v1: %d v2: %d", m_trackingId, headingDist, m_avgDeltaHeadingValid, m_lastAvgDeltaHeadingValid);

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
                                LOG_INFO("TrajectoryAnalyzer", "Wave count %d for point #%d", m_numWaveInflections, m_trackingId);
                                if (m_numWaveInflections == m_minWaveInflectionsForGesture)
                                {
                                    LOG_INFO("TrajectoryAnalyzer", "Wave gesture detected for point #%d", m_trackingId);
                                    m_isWaveGesture = true;
                                }
                            }
                        }
                    }
                    else if (degreeDifference > m_maxHeadingDiffForContinuation)
                    {
                        reset_wave();
                    }
                }
                if (m_avgDeltaHeadingValid)
                {
                    float recentDegreeDifference = get_degree_difference(m_accumulatedDeltaHeading, m_recentDeltaHeading);

                    if (recentDegreeDifference > m_minHeadingDiffForInflection)
                    {
                        set_for_next_inflection();
                    }
                }
            }
        }
        else if (m_avgDeltaHeadingValid)
        {
            set_for_next_inflection();
        }

        if (delta < m_maxSteadyDelta)
        {
            ++m_numSteadyFrames;
            if (!m_pointSteady && m_numSteadyFrames > m_minSteadyFrames)
            {
                m_pointSteady = true;

                LOG_INFO("TrajectoryAnalyzer", "Steady gesture detected for point #%d", m_trackingId);

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
        float headingDist = static_cast<float>(cv::norm(currentWorldPosition - m_headingTrackStart));
        bool validDist = headingDist > m_minHeadingDist;
        return validDist;
    }

    float TrajectoryAnalyzer::get_degree_difference(cv::Point3f& v1, cv::Point3f& v2)
    {
        float len1 = static_cast<float>(cv::norm(v1));
        float len2 = static_cast<float>(cv::norm(v2));

        if (len1 < EPSILON || len2 < EPSILON)
        {
            return 0.0f;
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
