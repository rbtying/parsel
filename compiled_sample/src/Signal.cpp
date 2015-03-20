#include "Signal.h"
#include <sndfile.hh>
#include <memory>
#include <iostream>

// TODO: do this better
#define BUFFER_SECS 0.01

using namespace psl;

// so jank but i had to put it on the heap to pass the reference
Signal::Signal(std::string filepath) :
    Signal(*std::auto_ptr<SndfileHandle>(new SndfileHandle(filepath)))
{ }

Signal::Signal(SndfileHandle& file) :
    Signal(fillFromFile(file), file.samplerate(), file.channels())
{ }

Signal::Signal(fill_t fill, int sampleRate, int channels) :
    buffer_(sampleRate * BUFFER_SECS, sample_t(channels, 0)),
    fill_(fill),
    sampleRate_(sampleRate),
    cacheB_(),
    firstRun_(true),
    more_(true),
    channels_(channels)
{ }

bool Signal::fillBuffer(bool B)
{
    if(B != cacheB_ || firstRun_)
    {
        more_ = fill_(&buffer_, B);
        cacheB_ = B;
        firstRun_ = false;
    }
    return more_;
}

int Signal::sampleRate() const
{
    return sampleRate_;
}

int Signal::channels() const
{
    return channels_;
}

fill_t Signal::fillFromFile(SndfileHandle& file)
{
    std::shared_ptr<std::vector<short>> fileDataP(
            new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());

    std::shared_ptr<int> frameP(new int(0));
    int channels = file.channels();
    return [channels, fileDataP, frameP](buffer_t* bufferP, bool) 
    {
        // fill buffer from fileDataP
        int stopPos = std::min(bufferP->size(),
                fileDataP->size() / channels - *frameP);
        for(int f = 0; f < stopPos; f++)
        {
            (*bufferP)[f] = std::vector<std::complex<double>>(channels);
            for(int c = 0; c < channels; c++)
                (*bufferP)[f][c] = fileDataP->at(((*frameP + f) * channels) + c);   
        }
        *frameP += stopPos;
        for(int i = stopPos; i < bufferP->size(); i++)
            (*bufferP)[i] = std::vector<std::complex<double>>(channels, 0);

        return stopPos == bufferP->size();
    };
}
