#ifndef SIMPLEVIEWER_H
#define SIMPLEVIEWER_H

#define MAX_DEPTH 10000

#include <SenseKit.h>
#include <streams/depth_types.h>
#include <stddef.h>

typedef struct
{
    /* Red value of this pixel. */
    uint8_t r;
    /* Green value of this pixel. */
    uint8_t g;
    /* Blue value of this pixel. */
    uint8_t b;
} RGB888Pixel;

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
    static void glutIdle();
    static void glutDisplay();
    static void glutKeyboard(unsigned char key, int x, int y);

    float                   m_pDepthHist[MAX_DEPTH];
    char                    m_strSampleName[255];
    unsigned int            m_nTexMapX;
    unsigned int            m_nTexMapY;
    RGB888Pixel*            m_pTexMap;
    int                     m_width;
    int                     m_height;

    sensekit_streamset_t* m_sensor;
    sensekit_depthstream_t* m_depthStream;
    sensekit_depthframe_t* m_depthFrame;
};


#endif /* SIMPLEVIEWER_H */
