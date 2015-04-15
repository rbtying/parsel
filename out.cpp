#include <iostream>
#include "outputs.h"



psl::Chunk<interval, interval, fsignal> applyFilterF;
psl::Chunk<psl::Signal, psl::Signal, psl::Chunk<interval, time, interval>, time> intervalMap;
psl::Chunk<float, time, time, time> envelope;
psl::Chunk<fsignal, freq> leftRightFilter;
psl::Chunk<interval, interval> filterInterval;
psl::Chunk<std::tuple<psl::Chunk<psl::Signal>>> out;

int main(int argc, char **argv) {
    applyFilterF = [&](interval input, fsignal filter) {
        return std::make_tuple(interval(ft(), (input()).stop(), (input()).start())(psl::multiply(ft(input()), filter())));
    };
    intervalMap = [&](psl::Signal input, psl::Chunk<interval, time, interval> f, time width) {
        return merge(map(chop(input(), width()), f()), width());
    };
    envelope = [&](time t, time decay, time attack) {
        return [&]() {
            if(psl::lessThan(t(), attack())()) {
                return psl::divide(t(), attack());
            }
            else {
                return [&]() {
                    if(psl::lessThan(t(), decay())()) {
                        return psl::minus(1.0, psl::divide(std::make_tuple(psl::minus(t(), attack()))(), decay()));
                    }
                    else {
                        return []() { return 0.0; };
                    }
                };
            }
        };
    };
    leftRightFilter = [&](freq cutoff) {
        return fsignal(lambda tsyms t expr());
    };
    filterInterval = [&](interval input) {
        return [&]() {
            psl::Chunk<freq, time> cutoff;
            psl::Chunk<freq> thing;

            cutoff = [&](time t) {
                return psl::multiply(envelope(t(), 10.0, 0.0), thing());
            };
            thing = [&]() {
                return []() { return 1000.0; };
            };

            return applyFilterF(input(), leftRightFilter(cutoff((input()).start())));
        }();
    };
    out = [&]() {
        return intervalMap(signalFromWav(input()), filterInterval(), 10.0);
    };

    bool B = true;
    while(out().get(0)().fillBuffer(B))
        B = !B;
}
