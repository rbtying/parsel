#include "Signal.h"
#include <sndfile.hh>
#include <memory>

using namespace psl;


Signal::Signal(std::string filename) :
    Signal(fillFromFile(filename))
{ }

Signal::Signal(fill_t fill) :
    fill_(fill)
{ }

void Signal::fillBuffer()
{
    fill_(buffer_);
}

fill_t Signal::fillFromFile(std::string filename)
{
    SndfileHandle file(filename);

    auto fileDataP = std::shared_ptr<std::vector<short>>(
            new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());

    auto frameP = std::shared_ptr<int>(0);
    int channels = file.channels();
    return [channels, fileDataP, frameP](buffer_t& buffer) 
    {
        // fill buffer from fileDataP
        int stopPos = std::min(buffer.size(), (fileDataP->size()/channels) - *frameP);
        for(int f = 0; f < stopPos; f++)
        {
            buffer[f] = std::vector<std::complex<double>>(channels);
            for(int c = 0; c < channels; c++)
                buffer[f][c] = fileDataP->at(((*frameP + f) * channels) + c);   
        }
        *frameP += stopPos;
        for(int i = stopPos; i < buffer.size(); i++)
            buffer[i] = std::vector<std::complex<double>>(channels, 0);
    };
}
