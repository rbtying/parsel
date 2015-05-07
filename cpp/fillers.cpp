#include "fillers.h"

#include <cassert>
#include <sndfile.hh>
#include <iostream>
#include <memory>
#include <fstream>
#include <sstream>

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

            for(int s = 0; s < stopPos; s++) {
                for(int c = 0; c < channels; c++) {
                    fbufferP->push_back(std::real((*(signal().buffer_))[s][c]));
                }
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

fill_t psl::fillFromFunction(op_tc f, int sampleRate, int channels)
{

    std::shared_ptr<bool> moreP(new bool(true));

    return [sampleRate, channels, f] (buffer_t* bufferP, bool B)
    {
        // for now
    	int stopPos = bufferP->size();

    	for (int s = 0; s < stopPos; s++)
    	{
    	    (*bufferP)[s] = std::vector<std::complex<double>>(channels);
    	    for(int c = 0; c < channels; c++)

    	    	// trying to get from sample -> time....
    	    	(*bufferP)[s][c] = f(s / sampleRate);
    	}

    	return stopPos == bufferP->size();

    };
}

fill_t psl::fillFromFile(SndfileHandle& file)
{
    std::shared_ptr<std::vector<short>> fileDataP(
    	    new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());

    std::shared_ptr<int> frameP(new int(0));
    int channels = file.channels();

    return [channels, fileDataP, frameP](buffer_t* bufferP, bool)
    {
    	int stopPos = std::min(bufferP->size(), fileDataP->size() / channels - *frameP);
    	for (int f = 0; f < stopPos; f++)
    	{
    	    (*bufferP)[f] = std::vector<std::complex<double>>(channels);
    	    for(int c = 0; c < channels; c++)
    	    {
    	    	(*bufferP)[f][c] = fileDataP->at(((*frameP + f) * channels) + c);
    	    }
    	}
        *frameP += stopPos;
    	for(int i = stopPos; i < bufferP->size(); i++)
    	{
    	    (*bufferP)[i] = std::vector<std::complex<double>>(channels, 0);
        }
    	return stopPos == bufferP->size();
    };
}

fill_t psl::fillFromOperator(op_ta f, Signal* lhs, Signal* rhs)
{
    std::shared_ptr<bool> moreP(new bool(true));

    // fail if two signals arent the same
    assert(lhs->channels() == rhs->channels());
    assert(lhs->sampleRate() == rhs->sampleRate());

    return [lhs, rhs, moreP, f](buffer_t* bufferP, bool B)
    {
	std::ofstream myfile;
	myfile.open("temp.txt");
	bool l_ok = lhs->fillBuffer(B);
	bool r_ok = rhs->fillBuffer(B);

	// TODO: shorter signal needs to be filled with 0's
	int stopPos = std::min(lhs->buffer_->size(), rhs->buffer_->size());
	int channels = lhs->channels();
	if (l_ok && r_ok)
	{
	    for (int s = 0; s < stopPos; s++)
	    {
		(*bufferP)[s] = std::vector<std::complex<double>>(channels);
		for (int c = 0; c < channels; c++)
		{
		    (*bufferP)[s][c] = f((*(lhs->buffer_))[s][c], (*(rhs->buffer_))[s][c]);
		}
	    }
	    myfile.close();
	    return *moreP;
	}
	else {
	    return *moreP = false;
	}
    };
}

fill_t psl::fillFromOperator(op_tb f, Signal* lhs)
{
    std::shared_ptr<bool> moreP(new bool(true));


    return [lhs, moreP, f](buffer_t* bufferP, bool B)
    {
	bool l_ok = lhs->fillBuffer(B);

	int stopPos = lhs->buffer_->size();
	int channels = lhs->channels();
	if (l_ok)
	{
	    for (int s = 0; s < stopPos; s++)
	    {
		(*bufferP)[s] = std::vector<std::complex<double>>(channels);
		for (int c = 0; c < channels; c++)
		{
		    (*bufferP)[s][c] = f((*(lhs->buffer_))[s][c]);
		}
	    }
	    return *moreP;
	}
	else {
	    return *moreP = false;
	}
    };
}

fill_t psl::fillFromPhaseShift(utime_t delay, Signal* sig)
{
    std::shared_ptr<bool> moreP(new bool(true));

    return [delay, sig, moreP](buffer_t* bufferP, bool B)
    {

	int stopPos = sig->buffer_->size();
	int channels = sig->channels();
	int startPos = abs(delay) * sig->sampleRate();

	bool success = sig->fillBuffer(B);
	if (success)
	{
	    if (delay >= 0) {
		for(int s = 0; s < startPos; s++)
		    (*bufferP)[s] = std::vector<std::complex<double>>(channels, 0);
		for (int s = startPos; s < stopPos; s++)
		{
		    (*bufferP)[s] = std::vector<std::complex<double>>(channels);
		    for (int c = 0; c < channels; c++)
		    {
			(*bufferP)[s][c] = (*(sig->buffer_))[s][c];
		    }
		}

	    } else {
		for (int s = 0; s < stopPos-startPos; s++) {

		    (*bufferP)[s] = std::vector<std::complex<double>>(channels);
		    for (int c = 0; c < channels; c++)
		    {
			(*bufferP)[s][c] = (*(sig->buffer_))[s+startPos][c];
		    }
		}
	    }
	    return *moreP;
	}
	else
	    return *moreP = false;
    };
}

