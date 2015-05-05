#include <iostream>
#include <functional>
#include "includes.h"

psl::Chunk<char> chr2Chunk(char chr) { psl::Chunk<char> c([=]{return chr;}); return c; }




psl::Chunk<std::function<std::tuple<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>> out;

int main(int argc, char **argv) {
                psl::Chunk<std::function<double(psl::Chunk<double>)>> g;
                psl::Chunk<std::function<double(psl::Chunk<double>)>> f;

                psl::set(g, psl::toChunk([&]() {
                        return [&](psl::Chunk<double> x) {
                        return psl::toChunk([&]() {
                            return psl::apply(psl::multiply, psl::toChunk([&]() {
                                    return 2.0;
                                    }), x);
                            })();
                        };
                        }));
                psl::set(f, psl::toChunk([&]() {
                        return [&](psl::Chunk<double> x) {
                        return psl::toChunk([&]() {
                            return psl::apply(psl::plus, x, psl::toChunk([&]() {
                                    return 1.0;
                                    }));
                            })();
                        };
                        }));

    psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];
    for (int i = 0; i < argc; i++) {
        std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);
        std::transform(argv[i], argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);
        psl::set(args[i], psl::toChunk([=]{ return chk; }));
    }

    bool B = false, success;
    do {
        B = !B;
        auto fc = out()(args[1]);
        success = std::get<0>(fc)().fillBuffer(B);
    } while (success);
    return 0;
}
