#include "FSignal.h"

#include <cassert>
#include <memory>
#include "fft4g.c"
#include <iostream>
#include <algorithm>

namespace psl {
    fill_t fillFromFrequency(Chunk<FSignal> fsignal)
    {
        std::shared_ptr<Chunk<FSignal>> fsigptr = std::make_shared<Chunk<FSignal>>(fsignal);

        return [fsigptr](buffer_t* bufferP, bool B) {
            Chunk<FSignal> fsig = *fsigptr;
            bool succ = fsig().fillBuffer(B);
            if (!succ) {
                return false;
            }

            for (buffer_t::const_iterator iter = fsig().timeSpace_->begin(); iter != fsig().timeSpace_->end(); ++iter) {
                (*bufferP)[iter - fsig().timeSpace_->begin()].assign(iter->begin(), iter->end());
            }
            return fsig().timeSpace_->size() == bufferP->size();
        };
    }

    FSignal::FSignal(freq_fill_t freq_f, int sampleRate, int channels) :
        freq_f_(freq_f),
        consistent_(std::shared_ptr<bool>(new bool(false))),
        bins_(std::shared_ptr<long>(new long(0))),
        sampleRate_(std::shared_ptr<int>(new int(sampleRate))),
        channels_(std::shared_ptr<int>(new int(channels))),
        timeSpace_(std::shared_ptr<buffer_t>(new buffer_t())),
        freqSpace_(std::shared_ptr<fbuffer_t>(new fbuffer_t())),
        position_(std::shared_ptr<long>(new long(0)))
    { }

    FSignal::FSignal(Chunk<Signal> sig, utime_t timestep) :
        FSignal(fillFromSignal(sig, timestep), sig().sampleRate(), sig().channels())
    { }

    FSignal::FSignal(const FSignal& copy) :
        freq_f_(copy.freq_f_),
        consistent_(copy.consistent_),
        bins_(copy.bins_),
        timeSpace_(copy.timeSpace_),
        freqSpace_(copy.freqSpace_),
        sampleRate_(copy.sampleRate_),
        channels_(copy.channels_),
        position_(copy.position_)
    { }

    FSignal& FSignal::operator=(const FSignal& other)
    {
        freq_f_ = other.freq_f_;
        consistent_ = other.consistent_;
        bins_ = other.bins_;
        timeSpace_ = other.timeSpace_;
        freqSpace_ = other.freqSpace_;
        sampleRate_ = other.sampleRate_;
        channels_ = other.channels_;
        position_ = other.position_;
    }

    void FSignal::computeTransform() {
        if (*consistent_) {
            return;
        }

        long bufsize = freqSpace_->size();
        assert(bufsize && !(bufsize & (bufsize - 1)));
        long n = bufsize * 2;
        long sqrt_n = static_cast<long>(std::sqrt(n) + 1);
        int ip[sqrt_n + 2];
        double a[n + 1], w[n * 5 / 4];
        ip[0] = 0; // ask cdft to initialize w

        timeSpace_->clear();
        timeSpace_->resize(bufsize);

        for (int channel = 0; channel < *channels_; ++channel)
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
                auto v = std::complex<double>(a[2 * k], a[2 * k + 1]);

                (*timeSpace_)[k].push_back(v);
            }
        }

        *consistent_ = true;
    }

    bool FSignal::fillBuffer(bool B) {
        *consistent_ = false;
        bool success = freq_f_(&(*freqSpace_), (*position_ * 1.0) / sampleRate(), B);
        if (success) {
            computeTransform();
        }
        *position_ += freqSpace_->size();
        *consistent_ = true;
        return success;
    }

    freq_fill_t FSignal::fillFromSignal(Chunk<Signal> sig2, utime_t timestep)
    {
        std::shared_ptr<Interval> itvlptr = std::make_shared<Interval>(sig2, timestep);
        std::shared_ptr<bool> succ = std::make_shared<bool>(true);
        return [itvlptr, succ](fbuffer_t* buf, double t, bool B) {
            bool b2 = *succ;

            long bufsize = itvlptr->buffer_.size();
            assert(bufsize && !(bufsize & (bufsize - 1)));
            long n = bufsize * 2;
            long sqrt_n = static_cast<long>(std::sqrt(n) + 1);
            int ip[sqrt_n + 2];
            double a[n + 1], w[n * 5 / 4];
            ip[0] = 0; // ask cdft to initialize w

            buf->clear();
            buf->resize(bufsize);

            for (int channel = 0; channel < itvlptr->channels(); ++channel)
            {
                long idx = 0;
                for (buffer_t::const_iterator iter = itvlptr->buffer_.begin(); iter != itvlptr->buffer_.end(); ++iter)
                {
                    const sample_t samp = *iter;
                    a[2 * idx] = samp[channel].real();
                    a[2 * idx + 1] = samp[channel].imag();
                    ++idx;
                }

                cdft(n, 1, a, ip, w);

                // a now contains the frequency-domain interpretation
                for (int k = 0; k < bufsize; ++k) {
                    auto v = std::complex<double>(a[2 * k], a[2 * k + 1]);
                    v *= 1.0 / bufsize;
                    (*buf)[k].push_back(v);
                }
            }
            assert(buf->size() == bufsize);

            *succ = itvlptr->advance();
            return b2;
        };
    }

    freq_fill_t fillFromOperator(binop_t f, Chunk<FSignal> lhs, Chunk<FSignal> rhs)
    {
        assert(lhs().channels() == rhs().channels());
        assert(lhs().sampleRate() == rhs().sampleRate());

        return [lhs, rhs, f](fbuffer_t* buf, double t, bool B) mutable
        {
            bool l_ok = lhs().fillBuffer(B);
            bool r_ok = rhs().fillBuffer(B);

            auto lfs = lhs().freqSpace_;
            auto rfs = rhs().freqSpace_;

            assert(lfs->size() == rfs->size());

            buf->clear();
            buf->resize(lfs->size());

            for (int s = 0; s < lfs->size(); ++s) {
                for (int c = 0; c < lhs().channels(); ++c) {
                    (*buf)[s].push_back(f((*lfs)[s][c], (*rfs)[s][c]));
                }
            }

            return l_ok && r_ok;
        };
    }

    FSignal operator*(Chunk<FSignal> fs1, Chunk<FSignal> fs2)
    {
        binop_t f = [](auto a, auto b) { return a * b; };
        return FSignal(fillFromOperator(f, fs1, fs2), fs1().sampleRate(), fs1().channels());
    }
}
