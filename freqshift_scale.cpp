#include <iostream>
#include <functional>
#include "includes.h"

psl::Chunk<char> chr2Chunk(char chr) { psl::Chunk<char> c([=]{return chr;}); return c; }




psl::Chunk<std::function<std::vector<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>> out;

int main(int argc, char **argv) {
    psl::set(out, psl::toChunk([=]() mutable {
                return std::function<std::vector<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>([=](psl::Chunk<std::vector<psl::Chunk<char>>> f1) mutable {
                    return psl::toChunk([=]() mutable {
                        return psl::toVector({psl::toChunk([=]() mutable {
                                return psl::apply(ift, psl::toChunk([=]() mutable {
                                        Chunk<FSignal> f1 = psl::toChunk([=]() mutable { return psl::apply(psl::ft, psl::toChunk([=]() mutable {
                                                return psl::apply(signal, psl::toChunk([=]() mutable {
                                                        return std::function<std::complex<double>(psl::Chunk<double>)>([=](psl::Chunk<double> t) mutable {
                                                            return psl::toChunk([=]() mutable {
                                                                return psl::apply(psl::sin, psl::toChunk([=]() mutable {
                                                                        return psl::apply(psl::multiply, t, psl::toChunk([=]() mutable {
                                                                                return 3300.0;
                                                                                }));
                                                                        }));
                                                                })();
                                                            });
                                                        }));
                                                }));
                                            });
                                        return freqShift(f1, toChunk([] () { return dubop_t( [](auto t) { return 1000 * std::sin(t()); }); } ) );
                                        }));
                                })});
                        })();
                    });
    }));

    psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];
    for (int i = 0; i < argc; i++) {
        std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);
        std::transform(argv[i], argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);
        psl::set(args[i], psl::toChunk([=]{ return chk; }));
    }

    bool B = false, success;
    auto fc = out()(args[1]);
    psl::Chunk<psl::Signal> writer1(psl::makeWriter(args[argc+(-2)], fc[0], args[argc-1]));
    do {
        B = !B;
        success = writer1().fillBuffer(B);
    } while (success);
    return 0;
}
