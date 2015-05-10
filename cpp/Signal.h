#pragma once

#include <vector>
#include <complex>
#include <functional>
#include <memory>

#include "Chunk.h"

// forward declaration
class SndfileHandle;

namespace psl
{
    typedef std::vector<std::complex<double>> sample_t;
    typedef std::vector<sample_t> buffer_t;
    typedef std::function<std::complex<double>(std::complex<double>,
            std::complex<double>)> binop_t;
    typedef std::function<std::complex<double>(std::complex<double>)> unop_t;
    typedef std::function<std::complex<double>(Chunk<double>)> dubop_t;


    typedef unsigned long utime_t;

    // returns false if unable to continue filling
    typedef std::function<bool(buffer_t*, bool)> fill_t;

    class Signal
    {
    public:
        Signal(std::function<fill_t()> fill_generator, int sampleRate, int channels);
        Signal(const Signal& copy);
        Signal& operator=(const Signal& other);

        Signal(std::string filepath);

        bool fillBuffer(bool B);

        int sampleRate() const;
        int channels() const;

        friend Signal operator+(Chunk<Signal> s1, Chunk<Signal> s2);
        friend Signal operator-(Chunk<Signal> s1, Chunk<Signal> s2);
        friend Signal operator*(Chunk<Signal> s1, Chunk<Signal> s2);
        friend Signal operator/(Chunk<Signal> s1, Chunk<Signal> s2);

        // how to implement these with operator+ for complex as well
        Signal add(double s);
        Signal sub(double s);
        Signal mul(double s);
        Signal div(double s);

        Signal shift(utime_t delay);

        fill_t fill_;
        std::function<fill_t()> fill_gen_;
        std::shared_ptr<buffer_t> buffer_;

    private:
        Signal(SndfileHandle& file);

        std::shared_ptr<bool> cacheB_, firstRun_, more_;
        int sampleRate_, channels_;

        Chunk<Signal> chunk();

    };

    Signal operator+(Chunk<Signal> s1, Chunk<Signal> s2);
    Signal operator-(Chunk<Signal> s1, Chunk<Signal> s2);
    Signal operator*(Chunk<Signal> s1, Chunk<Signal> s2);
    Signal operator/(Chunk<Signal> s1, Chunk<Signal> s2);
}

