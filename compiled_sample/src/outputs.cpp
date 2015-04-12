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

fill_t psl::fillFromFile(SndfileHandle& file)
{
    std::shared_ptr<std::vector<short>> fileDataP(
    	    new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());
    
    std::shared_ptr<int> frameP(new int(0));
    int channels = file.channels();

    return [channels, fileDataP, frameP](buffer_t* bufferP, bool)
    {
    	int stopPos = std::min(bufferP->size(),
    		fileDataP->size() / channels - *frameP);
    	for (int f = 0; f < stopPos; f++)
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

fill_t psl::fillFromOperator(op_t f, Signal* lhs, Signal* rhs)
{
    std::shared_ptr<bool> moreP(new bool(true));


    return [lhs, rhs, moreP, f](buffer_t* bufferP, bool B)
    {
	bool l_ok = lhs->fillBuffer(B);
	bool r_ok = rhs->fillBuffer(B);

	int stopPos = lhs->buffer_.size();
	int channels = lhs->channels();
	if (l_ok && r_ok) 
	{
	    for (int s = 0; s < stopPos; s++)
	    {
		(*bufferP)[s] = std::vector<std::complex<double>>(channels);
		for (int c = 0; c < channels; c++)
		{
		    (*bufferP)[s][c] = f(lhs->buffer_[s][c], rhs->buffer_[s][c]);		
		}
	    }
	return *moreP;
	}
	else
	    return *moreP=false;
    };
}

