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

// can we include a constructor with default sample rate, channels??
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

Signal operator^(const Signal& lhs, const Signal& rhs) {

}
Signal operator/(const Signal& lhs, const Signal& rhs) {

}
Signal operator*(const Signal& lhs, const Signal& rhs) {

}
Signal operator+(const Signal& lhs, const Signal& rhs) {
    if (lhs.sampleRate() != rhs.sampleRate() ) {
	// Need resample method
	// can we have a default sample rate?
    }

    // this constructor doesn't exist yet....
    return new Signal(fillFromOperator('+', lhs, rhs))
    
}
Signal operator-(const Signal& lhs, const Signal& rhs) {
    if (lhs.sampleRate() != rhs.sampleRate() ) {
	// Need resample method
	// can we have a default sample rate?
    }

    // this constructor doesn't exist yet....
    return new Signal(fillFromOperator('-', lhs, rhs))

}

fill_t Signal::fillFromOperator(char op, const Signal& lhs, const Signal& rhs) 
{
 
    std::shared_ptr<int> frameP(new int(0));

    // assuming lhs and rhs are the same size, have the same channels, sample rate etc
    int channels = lhs.channels();
    
    return [channels, lhs.buffer_, rhs.buffer_, frameP](buffer_t* bufferP, bool)
    {
	// or actually, buffers are fixed size, right?	
	int stopPos = std:min(bufferP->size(), lhs.buffer_.size()); 
		
	for (int f = 0; f < stopPos; f++) 
	{
	    (*bufferP)[f] = std:vector<std::complex<double>>(channels);

	    for (int c = 0; c < channels; c++)
	    {
	    	if (op == '+') {
		    (*bufferP)[f][c] = lhs.buffer_[f][c] + rhs.buffer_[f][c];
		} else if (op == '-') {
		    //
		} else
		    // throw runtime exception?
	    }

	}

        *frameP += stopPos;
        for(int i = stopPos; i < bufferP->size(); i++)
            (*bufferP)[i] = std::vector<std::complex<double>>(channels, 0);

        return stopPos == bufferP->size();

    };
    

}


fill_t Signal::fillFromFile(SndfileHandle& file)
{
    std::shared_ptr<std::vector<short>> fileDataP(
            new std::vector<short>(file.channels() * file.frames()));
    file.read(fileDataP->data(), fileDataP->size());

    std::shared_ptr<int> frameP(new int(0));
    int channels = file.channels();

    // what if the file is bigger than the buffer? Don't we need to loop through the file somewhere?
    
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
