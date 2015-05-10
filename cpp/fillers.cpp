#include "fillers.h"
#include "utils.h"

#include <cassert>
#include <sndfile.hh>
#include <iostream>
#include <memory>
#include <fstream>
#include <sstream>
#include <climits>

using namespace psl;

fill_t psl::toWavFile(psl::Chunk<Signal> signal, std::string filepath, float seconds)
{
    int format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    int channels = signal().channels();
    std::shared_ptr<SndfileHandle> fileP(new SndfileHandle(filepath,
                SFM_WRITE, format, channels, signal().sampleRate()));
    std::shared_ptr<int> samplesP(new int(0));
    std::shared_ptr<std::vector<short>> fbufferP(new std::vector<short>());
    std::shared_ptr<bool> moreP(new bool(true));

    return [fileP, fbufferP, signal, moreP, seconds](buffer_t* bufferP, bool B) mutable
    {
        signal().fillBuffer(B);
        int totalSamples = seconds * signal().sampleRate();
        int channels = signal().channels();
        if(fbufferP->size() < totalSamples * channels)
        {
            int stopPos = std::min(totalSamples - fbufferP->size() / channels,
                    signal().buffer_->size());

            for(int s = 0; s < stopPos; s++)
                for(int c = 0; c < channels; c++)
                {
                    double val = std::real((*signal().buffer_)[s][c]);
                    if(val > 1)
                        val = 1;
                    else if(val < -1)
                        val = -1;

                    fbufferP->push_back(val * SHRT_MAX);
                }
        }
        else if(*moreP)
        {
            fileP->write(fbufferP->data(), fbufferP->size());
            *moreP = false;
        }

        int bufferSize = bufferP->size();
        for(int s = 0; s < bufferSize; s++)
            for(int c = 0; c < channels; c++)
                (*bufferP)[s][c] = (*(signal().buffer_))[s][c];

        return *moreP;
    };
}

fill_t psl::fillFromFunction(Chunk<dubop_t> f, int sampleRate, int channels)
{
    std::shared_ptr<int> s = std::make_shared<int>(0);
    return [s, sampleRate, channels, f] (buffer_t* bufferP, bool B) mutable
    {
        int startS = *s;
        int stopPos = *s + bufferP->size();

        for (; *s < stopPos; (*s)++)
        {
            int sample = *s;
            Chunk<double> time = toChunk([sample, sampleRate]()
                    { return (double)sample / sampleRate; });
            std::complex<double> val = f()(time);

            (*bufferP)[sample - startS] = std::vector<std::complex<double>>(channels, val);
        }

        return *s >= stopPos;
    };
}

fill_t psl::fillFromFile(SndfileHandle& file)
{
    std::shared_ptr<std::vector<short>> fileDataP(
            new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());

    std::shared_ptr<int> frameP(new int(0));
    int channels = file.channels();

    return [channels, fileDataP, frameP](buffer_t* bufferP, bool) mutable
    {
        int stopPos = std::min(bufferP->size(), fileDataP->size() / channels - *frameP);
        for (int f = 0; f < stopPos; f++)
        {
            (*bufferP)[f] = std::vector<std::complex<double>>(channels);
            for(int c = 0; c < channels; c++)
                (*bufferP)[f][c] = (double)fileDataP->at(((*frameP + f) * channels) + c) /
                        SHRT_MAX;
        }
        *frameP += stopPos;
        for(int i = stopPos; i < bufferP->size(); i++)
            (*bufferP)[i] = std::vector<std::complex<double>>(channels, 0);

        return stopPos == bufferP->size();
    };
}

fill_t psl::fillFromOperator(binop_t f, Chunk<Signal> lhs, Chunk<Signal> rhs)
{
    // fail if two signals arent the same
    assert(lhs().channels() == rhs().channels());
    assert(lhs().sampleRate() == rhs().sampleRate());

    return [lhs, rhs, f](buffer_t* bufferP, bool B) mutable
    {
        bool l_ok = lhs().fillBuffer(B);
        bool r_ok = rhs().fillBuffer(B);

        int stopPos = std::min(lhs().buffer_->size(), rhs().buffer_->size());
        int channels = lhs().channels();

        for (int s = 0; s < stopPos; s++)
        {
            (*bufferP)[s] = std::vector<std::complex<double>>(channels);
            for(int c = 0; c < channels; c++)
                (*bufferP)[s][c] = f((*(lhs().buffer_))[s][c],
                        (*(rhs().buffer_))[s][c]);
        }

        return l_ok && r_ok;
    };
}

fill_t psl::fillFromOperator(unop_t f, Chunk<Signal> signal)
{
    return [signal, f](buffer_t* bufferP, bool B) mutable
    {
        bool l_ok = signal().fillBuffer(B);

        int stopPos = signal().buffer_->size();
        int channels = signal().channels();
        for (int s = 0; s < stopPos; s++)
        {
            (*bufferP)[s] = std::vector<std::complex<double>>(channels);
            for (int c = 0; c < channels; c++)
                (*bufferP)[s][c] = f((*(signal().buffer_))[s][c]);
        }
        return l_ok;
    };
}

fill_t psl::fillFromPhaseShift(utime_t delay, Chunk<Signal> signal)
{
    return [delay, signal](buffer_t* bufferP, bool B) mutable
    {

        int stopPos = signal().buffer_->size();
        int channels = signal().channels();
        int startPos = abs(delay) * signal().sampleRate();

        bool success = signal().fillBuffer(B);

        if (delay >= 0)
        {
            for(int s = 0; s < startPos; s++)
                (*bufferP)[s] = std::vector<std::complex<double>>(channels, 0);
            for (int s = startPos; s < stopPos; s++)
            {
                (*bufferP)[s] = std::vector<std::complex<double>>(channels);
                for (int c = 0; c < channels; c++)
                    (*bufferP)[s][c] = (*(signal().buffer_))[s][c];
            }
        }
        else
            for (int s = 0; s < stopPos-startPos; s++)
            {
                (*bufferP)[s] = std::vector<std::complex<double>>(channels);
                for (int c = 0; c < channels; c++)
                    (*bufferP)[s][c] = (*(signal().buffer_))[s+startPos][c];
            }
        return success;
    };
}

