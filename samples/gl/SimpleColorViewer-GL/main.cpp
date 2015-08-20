#include "SimpleColorViewer.h"

int main(int argc, char** argv)
{
    astra::Astra::initialize();
    SimpleColorViewer colorViewer("Color Viewer");

    colorViewer.init(argc, argv);
    colorViewer.run();
}
