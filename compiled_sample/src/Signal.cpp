#include "Signal.h"

#include <soundfile.h>

using namespace psl;


Signal::Signal(std::string filename)
{
    int numSamples;

    SoundFileRead sf(filename.c_str());
    numSamples = sf.getSamples();
    buffer_ = buffer_t(numSamples, sample_t(2));

    if(sf.getChannels() == 1)
        for(int i = 0; i < numSamples; i++)
        {
            buffer_[i][1] = buffer_[i][0] = sf.getCurrentSampleDouble(0);
            sf.incrementSample(); 
        }
    else
        for(int i = 0; i < numSamples; i++)
        {
            buffer_[i][0] = sf.getCurrentSampleDouble(0);
            buffer_[i][1] = sf.getCurrentSampleDouble(1);
            sf.incrementSample(); 
        }
}

void Signal::fillBuffer()
{
}
