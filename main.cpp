#include <iostream>
#include <functional>
#include "includes.h"

psl::Chunk<char> chr2Chunk(char chr) { psl::Chunk<char> c([=]{return chr;}); return c; }




psl::Chunk<std::function<std::tuple<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>> out;

int main(int argc, char **argv) {
psl::set(out, psl::toChunk([&]() { return [&](psl::Chunk<std::vector<psl::Chunk<char>>> input) {
 return [&]() {
psl::Chunk<std::function<int(psl::Chunk<int>)>> g;
psl::Chunk<std::function<int(psl::Chunk<double>)>> f;

psl::set(g, psl::toChunk([&]() { return [&](psl::Chunk<int> x) {
 return psl::toChunk([&]() { return psl::toChunk([&]() { return psl::apply(psl::multiply, psl::toChunk([&]() { return 2.0; }), x); }); })();
}; }));
psl::set(f, psl::toChunk([&]() { return [&](psl::Chunk<double> x) {
 return psl::toChunk([&]() { return psl::toChunk([&]() { return psl::apply(psl::plus, x, psl::toChunk([&]() { return 1.0; })); }); })();
}; }));

return psl::toChunk([&]() { return psl::apply(psl::toSignal, psl::toChunk([&]() { return psl::apply(g, psl::toChunk([&]() { return psl::apply(f, psl::toChunk([&]() { return 5.0; })); })); })); })();
}();
}; }));

psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];
for (int i = 0; i < argc; i++) {
std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);
std::transform(argv[i], argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);
args[i] = [=]{ return chk; };
}

bool B = true;
while(out()(args[1])().fillBuffer(B))
B = !B;
return 0;
}