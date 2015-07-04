#ifndef SIMPLEVIEWER_H
#define SIMPLEVIEWER_H

#define MAX_DEPTH 10000

#include <SenseKit/SenseKit.h>
#include <SenseKitUL/SenseKitUL.h>
#include <stddef.h>
#include <math.h>

typedef struct
{
    /* Red value of this pixel. */
    uint8_t r;
    /* Green value of this pixel. */
    uint8_t g;
    /* Blue value of this pixel. */
    uint8_t b;
} RGB888Pixel;

struct Vector3
{
    float x;
    float y;
    float z;

    Vector3()
    {
        x = 0;
        y = 0;
        z = 0;
    }

    Vector3(float xVal, float yVal, float zVal)
    {
        x = xVal;
        y = yVal;
        z = zVal;
    }

    bool isEmpty()
    {
        return x == 0 && y == 0 && z == 0;
    }

    static float DotProduct(Vector3 lhs, Vector3 rhs)
    {
        float result = lhs.x * rhs.x + lhs.y * rhs.y + lhs.z * rhs.z;
        return result;
    }

    static Vector3 CrossProduct(Vector3 lhs, Vector3 rhs)
    {
        float x = (lhs.y * rhs.z) - (lhs.z * rhs.y);
        float y = (lhs.z * rhs.x) - (lhs.x * rhs.z);
        float z = (lhs.x * rhs.y) - (lhs.y * rhs.x);

        return Normalize(Vector3(x, y, z));
    }

    static Vector3 Normalize(Vector3 v)
    {
        double length = sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
        if (length < 1e-9)
        {
            return Vector3(0, 0, 0);
        }
        else
        {
            return Vector3(v.x / length, v.y / length, v.z / length);
        }
    }
};

class SampleViewer
{
public:
    SampleViewer(const char* strSampleName);
    virtual ~SampleViewer();
    virtual void init(int argc, char **argv);
    virtual void run();   //Does not return
protected:
    virtual void display();
    virtual void displayPostDraw(){};       // Overload to draw over the screen image

    virtual void onKey(unsigned char key, int x, int y);

    virtual void initOpenGL(int argc, char **argv);
    void initOpenGLHooks();

private:
    SampleViewer(const SampleViewer&);
    SampleViewer& operator=(SampleViewer&);

    static SampleViewer* ms_self;
    void initTextMap(int width, int height);
    void calculateNormals(sensekit_depthframe_t& frame, sensekit_image_metadata_t metadata);
    void showTex(int depthWidth, int depthHeight);
    void updateTex(sensekit_depthframe_t depthFrame, sensekit_image_metadata_t metadata);
    void CalculateHistogram(float* pHistogram, int histogramSize, sensekit_depthframe_t frame, sensekit_image_metadata_t metadata);
    static void glutIdle();
    static void glutDisplay();
    static void glutKeyboard(unsigned char key, int x, int y);

    float                   m_pDepthHist[MAX_DEPTH];
    char                    m_strSampleName[255];
    unsigned int            m_nTexMapX;
    unsigned int            m_nTexMapY;
    RGB888Pixel*            m_pTexMap;

    Vector3*                m_normalMap{ nullptr };
    Vector3*                m_blurNormalMap{ nullptr };
    size_t                  m_normalMapLen{ 0 };
    Vector3                 m_lightVector;
    RGB888Pixel             m_lightColor;
    RGB888Pixel             m_ambientColor;

    sensekit_streamsetconnection_t m_sensor;
    sensekit_reader_t m_reader;
    sensekit_depthstream_t m_depthStream;
};


#endif /* SIMPLEVIEWER_H */
