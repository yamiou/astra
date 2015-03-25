#ifndef UTILS_H
#define UTILS_H

#include <SenseKit.h>

#include <stdio.h>

#ifdef _WIN32
#include <conio.h>
int wasKeyboardHit()
{
        return (int)_kbhit();
}

#else // linux

#include <unistd.h>
#include <stdlib.h>
#include <termios.h>
#include <fcntl.h>

int wasKeyboardHit()
{
        struct termios oldt, newt;
        int ch;
        int oldf;

        // don't echo and don't wait for ENTER
        tcgetattr(STDIN_FILENO, &oldt);
        newt = oldt;
        newt.c_lflag &= ~(ICANON | ECHO);
        tcsetattr(STDIN_FILENO, TCSANOW, &newt);
        oldf = fcntl(STDIN_FILENO, F_GETFL, 0);

        // make it non-blocking (so we can check without waiting)
        if (0 != fcntl(STDIN_FILENO, F_SETFL, oldf | O_NONBLOCK))
        {
                return 0;
        }

        ch = getchar();

        tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
        if (0 != fcntl(STDIN_FILENO, F_SETFL, oldf))
        {
                return 0;
        }

        if(ch != EOF)
        {
                ungetc(ch, stdin);
                return 1;
        }

        return 0;
}

void Sleep(int millisecs)
{
        usleep(millisecs * 1000);
}
#endif // WIN32

void calculateHistogram(float* pHistogram, int histogramSize, sensekit_depthframe_t& frame)
{
        short* pDepth = frame.data;
        // Calculate the accumulative histogram (the yellow display...)
        memset(pHistogram, 0, histogramSize*sizeof(float));
        int height = frame.height;
        int width = frame.width;

        unsigned int nNumberOfPoints = 0;
        for (int y = 0; y < height; ++y)
        {
                for (int x = 0; x < width; ++x, ++pDepth)
                {
                        if (*pDepth != 0)
                        {
                                pHistogram[*pDepth]++;
                                nNumberOfPoints++;
                        }
                }
        }
        for (int nIndex=1; nIndex<histogramSize; nIndex++)
        {
                pHistogram[nIndex] += pHistogram[nIndex-1];
        }
        if (nNumberOfPoints)
        {
                for (int nIndex=1; nIndex<histogramSize; nIndex++)
                {
                        pHistogram[nIndex] = (256 * (1.0f - (pHistogram[nIndex] / nNumberOfPoints)));
                }
        }
}

#endif /* UTILS_H */
