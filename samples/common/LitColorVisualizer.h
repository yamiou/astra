#ifndef LITCOLORVISUALIZER_H
#define LITCOLORVISUALIZER_H

#include <Astra/astra_capi.h>
#include <AstraUL/streams/Point.h>
#include <AstraUL/Vector.h>
#include <cstring>
#include <algorithm>

namespace samples { namespace common {

	using namespace astra;

    class LitColorVisualizer
    {
    public:
        LitColorVisualizer()
        {
           
        }

        void update(ColorFrame& colorFrame);

		astra_rgb_pixel_t* get_output() { return m_outputBuffer.get(); }

    private:
       

        size_t m_outputWidth;
        size_t m_outputHeight;

		using BufferPtr = std::unique_ptr<astra_rgb_pixel_t[]>;
        BufferPtr m_outputBuffer{nullptr};

        void prepare_buffer(size_t width, size_t height);
    
    };


  

    void LitColorVisualizer::prepare_buffer(size_t width, size_t height)
    {
        if (m_outputBuffer == nullptr || width != m_outputWidth || height != m_outputHeight)
        {
            m_outputWidth = width;
            m_outputHeight = height;
			m_outputBuffer = std::make_unique<astra_rgb_pixel_t[]>(m_outputWidth * m_outputHeight);
        }

		std::fill(m_outputBuffer.get(), m_outputBuffer.get() + m_outputWidth*m_outputHeight, astra_rgb_pixel_t{ 0, 0, 0 });
    }

	void LitColorVisualizer::update(ColorFrame& colorFrame)
    {
	
		const size_t width = colorFrame.resolutionX();
		const size_t height = colorFrame.resolutionY();

        prepare_buffer(width, height);

		const astra_rgb_pixel_t* colorData = colorFrame.data();

		astra_rgb_pixel_t* texturePtr = m_outputBuffer.get();

        for (int y = 0; y < height; ++y)
        {
            for (int x = 0; x < width; ++x, ++colorData, ++texturePtr)
            {
				texturePtr->r = colorData->r;
				texturePtr->g = colorData->b;
				texturePtr->b = colorData->g;
				

            }
        }
    }

	
}}

#endif /* LITCOLORVISUALIZER_H */
