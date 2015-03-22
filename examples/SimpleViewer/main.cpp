#include "SimpleViewer.h"

int main(int argc, char** argv)
{
    SampleViewer sampleViewer("Simple Viewer");


    sampleViewer.init(argc, argv);
    sampleViewer.run();
}
