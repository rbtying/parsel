#pragma once

#include <vector>
#include <complex>
#include <functional>

// forward declaration
class SndfileHandle;

namespace psl
{
    typedef std::vector<std::complex<double>> sample_t;
    typedef std::vector<sample_t> buffer_t;
    typedef std::function<std::complex<double>(std::complex<double>, std::complex<double>)> op_ta;
    typedef std::function<std::complex<double>(std::complex<double>)> op_tb;


    typedef unsigned long utime_t;

    // returns false if unable to continue filling
    typedef std::function<bool(buffer_t*, bool)> fill_t;

    class Signal
    {
    public:
        Signal(std::string filepath);
        Signal(fill_t fill, int sampleRate, int channels);

        bool fillBuffer(bool B);

        int sampleRate() const;
        int channels() const;

	Signal add(Signal* s);
	Signal add(double s);
	
	Signal sub(Signal* s);
	Signal sub(double s);
	
	Signal mul(Signal* s);
	Signal mul(double s);
	
	Signal div(Signal* s);
	Signal div(double s);
	
	Signal shift(utime_t delay);

        fill_t fill_;
        buffer_t buffer_;

    private:
        Signal(SndfileHandle& file);

        bool cacheB_, firstRun_, more_;
        int sampleRate_, channels_;
    };
}
