#include "outputs.h"

#include <sndfile.hh>
#include <iostream>
#include <memory>

using namespace psl;

fill_t psl::toWavFile(Signal* signalP, std::string filepath, float seconds)
{
    int format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    int channels = signalP->channels();
    std::shared_ptr<SndfileHandle> fileP(new SndfileHandle(filepath,
                SFM_WRITE, format, channels, signalP->sampleRate()));
    std::shared_ptr<int> samplesP(new int(0));
    std::shared_ptr<std::vector<short>> fbufferP(new std::vector<short>());
    std::shared_ptr<bool> moreP(new bool(true));

    return [fileP, fbufferP, signalP, moreP, seconds](buffer_t* bufferP, bool B)
    {
        signalP->fillBuffer(B);
        int totalSamples = seconds * signalP->sampleRate();
        int channels = signalP->channels();
        if(fbufferP->size() < totalSamples * channels)
        {
            int stopPos = std::min(totalSamples - fbufferP->size() / channels,
                    signalP->buffer_.size());

            for(int s = 0; s < stopPos; s++)
                for(int c = 0; c < channels; c++)
                    fbufferP->push_back(std::real(signalP->buffer_[s][c]));
        }
        else if(*moreP)
        {
            fileP->write(fbufferP->data(), fbufferP->size());
            *moreP = false;
        }

        int bufferSize = bufferP->size();
        for(int s = 0; s < bufferSize; s++)
            for(int c = 0; c < channels; c++)
                (*bufferP)[s][c] = signalP->buffer_[s][c];

        return *moreP;
    };
}
