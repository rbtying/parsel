#include "FSignal.h"

#include <cassert>
#include <memory>
#include "fft4g.c"
#include <iostream>

namespace psl {
    fill_t fillFromFrequency(Chunk<FSignal> fsignal)
    {
        std::shared_ptr<Chunk<FSignal>> fsigptr = std::make_shared<Chunk<FSignal>>(fsignal);

        return [fsigptr](buffer_t* bufferP, bool B) {
            Chunk<FSignal> fsig = *fsigptr;
            bool succ = fsig().fillBuffer(B);
            if (!succ) {
                std::cout << "wah" << std::endl;
                return false;
            }

            for (buffer_t::const_iterator iter = fsig().timeSpace_->buffer_.begin(); iter != fsig().timeSpace_->buffer_.end(); ++iter) {
                bufferP->push_back(*iter);
            }
            return fsig().timeSpace_->buffer_.size() == bufferP->size();
        };
    }
    FSignal::FSignal(freq_fill_t freq_f) :
        freq_f_(freq_f),
        consistent_(std::shared_ptr<bool>(new bool(false))),
        bins_(std::shared_ptr<long>(new long(0))),
        timeSpace_(std::shared_ptr<Interval>(new Interval())),
        freqSpace_(std::shared_ptr<fbuffer_t>(new fbuffer_t()))
    { }

    FSignal::FSignal(Chunk<Signal> sig, utime_t timestep) :
        FSignal(fillFromSignal(sig, timestep))
    { }

    FSignal::FSignal(const FSignal& copy) :
        freq_f_(copy.freq_f_),
        consistent_(copy.consistent_),
        bins_(copy.bins_),
        timeSpace_(copy.timeSpace_),
        freqSpace_(copy.freqSpace_)
    { }

    FSignal& FSignal::operator=(const FSignal& other)
    {
        freq_f_ = other.freq_f_;
        consistent_ = other.consistent_;
        bins_ = other.bins_;
        timeSpace_ = other.timeSpace_;
        freqSpace_ = other.freqSpace_;
    }

    void FSignal::computeTransform() {
        if (*consistent_) {
            return;
        }

        const buffer_t buf = timeSpace_->buffer();
        long bufsize = buf.size();
        assert(bufsize && !(bufsize & (bufsize - 1)));
        long n = bufsize * 2;
        long sqrt_n = static_cast<long>(std::sqrt(n) + 1);
        int ip[sqrt_n + 2];
        double a[n + 1], w[n * 5 / 4];
        ip[0] = 0; // ask cdft to initialize w

        freqSpace_->clear();
        freqSpace_->resize(bufsize);

        for (int channel = 0; channel < timeSpace_->channels(); ++channel)
        {
            long idx = 0;
            for (buffer_t::const_iterator iter = buf.begin(); iter != buf.end(); ++iter)
            {
                const sample_t samp = *iter;
                a[2 * idx] = samp[channel].real();
                a[2 * idx + 1] = samp[channel].imag();
                ++idx;
            }

            cdft(n, 1, a, ip, w);

            // a now contains the frequency-domain interpretation
            for (int k = 0; k < bufsize; ++k) {
                (*freqSpace_)[k].push_back(std::complex<double>(a[2 * k], a[2 * k + 1]));
            }
        }
        *bins_ = n;

        timeSpace_->buffer_.clear();
        timeSpace_->buffer_.resize(bufsize);

        for (int channel = 0; channel < timeSpace_->channels(); ++channel)
        {
            long idx = 0;
            for (fbuffer_t::const_iterator iter = freqSpace_->begin(); iter != freqSpace_->end(); ++iter)
            {
                const fsample_t samp = *iter;
                a[2 * idx] = samp[channel].real();
                a[2 * idx + 1] = samp[channel].imag();
                ++idx;
            }

            cdft(n, -1, a, ip, w);

            // a now contains the time-domain interpretation
            for (int k = 0; k < bufsize; ++k) {
                timeSpace_->buffer_[k].push_back(std::complex<double>(a[2 * k], a[2 * k + 1]));
            }
        }

        *consistent_ = true;
    }

    bool FSignal::fillBuffer(bool B) {
        *consistent_ = false;
        bool success = freq_f_(&(*timeSpace_), B);
        if (success) {
            computeTransform();
        }
        return success;
    }

    freq_fill_t FSignal::fillFromSignal(Chunk<Signal> sig2, utime_t timestep)
    {
        std::shared_ptr<Chunk<Signal>> sigptr = std::make_shared<Chunk<Signal>>(sig2);
        return [sigptr, timestep](Interval* itvl, bool B) {
            Chunk<Signal> sig = *sigptr;
            // start after end of last interval
            utime_t start_time = itvl->end();
            utime_t end_time = start_time + timestep;

            long time_step_in_samples = timestep * sig().sampleRate() / 1000000;
            bool succ = sig().fillBuffer(B);
            if (!succ) {
                std::cout << "ahh" << std::endl;
                return false;
            }
            assert(sig().buffer_->size() >= time_step_in_samples);

            /* *itvl = Interval(sig, start_time, end_time); */
            *itvl = Interval(sig, 0, timestep); // streams don't keep track of old data -.-
            return true;
        };
    }
}
