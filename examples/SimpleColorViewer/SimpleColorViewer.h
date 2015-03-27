#ifndef SIMPLE_COLOR_VIEWER_H
#define SIMPLE_COLOR_VIEWER_H

#define MAX_DEPTH 10000

#include <SenseKit.h>
#include <SenseKitUL.h>
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

class SimpleColorViewer
{
public:
    SimpleColorViewer(const char* strSampleName);
    virtual ~SimpleColorViewer();

    virtual void init(int argc, char **argv);
    virtual void run();   //Does not return

protected:
    virtual void display();
    virtual void displayPostDraw(){};       // Overload to draw over the screen image

    virtual void onKey(unsigned char key, int x, int y);

    virtual void initOpenGL(int argc, char **argv);
    void initOpenGLHooks();

private:
    SimpleColorViewer(const SimpleColorViewer&);
    SimpleColorViewer& operator=(SimpleColorViewer&);

    static SimpleColorViewer* ms_self;
    static void glutIdle();
    static void glutDisplay();
    static void glutKeyboard(unsigned char key, int x, int y);

    char                    m_strSampleName[255];
    unsigned int            m_nTexMapX;
    unsigned int            m_nTexMapY;
    RGB888Pixel*            m_pTexMap;
    int                     m_width;
    int                     m_height;

    sensekit_streamset_t* m_sensor;
    sensekit_colorstream_t* m_colorStream;
    sensekit_colorframe_t* m_colorFrame;
};


#endif /* SIMPLE_COLOR_VIEWER_H */
