#pragma once

#include <vector>
#include <complex>
#include <functional>

#include "Signal.h"
#include "Interval.h"
#include "Chunk.h"

// declaration for complex dft
void cdft(int, int, double *, int *, double *);

namespace psl
{
    typedef std::function<bool(Interval*, bool)> freq_fill_t;
    typedef std::vector<std::complex<double>> fsample_t;
    typedef std::vector<fsample_t> fbuffer_t;

    class FSignal
    {
        public:
            FSignal(freq_fill_t freq_f);
            FSignal(Chunk<Signal> sig, utime_t timestep);
            FSignal(const FSignal& copy);
            FSignal& operator=(const FSignal& other);

            int bins() const { return *bins_; }
            int sampleRate() const { return timeSpace_->sampleRate(); }
            int channels() const { return timeSpace_->channels(); }
            int consistent() const { return *consistent_; }

            bool fillBuffer(bool B);

            std::shared_ptr<Interval> timeSpace_;
            std::shared_ptr<fbuffer_t> freqSpace_;

        private:
            void computeTransform();

            freq_fill_t fillFromSignal(Chunk<Signal> sig, utime_t timestep);

            freq_fill_t freq_f_;
            std::shared_ptr<bool> consistent_;
            std::shared_ptr<long> bins_;

            long NMAX_;
            long NMAXSQRT_;
    };

    fill_t fillFromFrequency(Chunk<FSignal> fsignal);
}
