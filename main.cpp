#include <iostream>
#include <functional>
#include "outputs.h"

psl::Chunk<char> chr2Chunk(char chr) { return [=]{return chr;}; }




psl::Chunk<std::function<std::tuple<psl::Chunk<psl::Signal>>(psl::Chunk<std::vector<psl::Chunk<char>>>)>> out;

int main(int argc, char **argv) {
out = [&]() { return [&](psl::Chunk<std::vector<psl::Chunk<char>>> input) {
 return [&]() {
psl::Chunk<std::function<int(psl::Chunk<int>)>> g;
psl::Chunk<std::function<int(psl::Chunk<char>,psl::Chunk<int>)>> f;

g = [&]() { return [&](psl::Chunk<int> x) {
 return [&]() { return [&]() { return psl::apply(psl::multiply, [&]() { return 2.0; }, x); }; }();
}; };
f = [&]() { return [&](psl::Chunk<char> c, psl::Chunk<int> x) {
 return [&]() { return [&]() { return psl::apply(psl::plus, x, [&]() { return 1.0; }); }; }();
}; };

return [&]() { return psl::apply(g, [&]() { return psl::apply(f, [&]() { return 5.0; }); }); }();
}();
}; };

psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];
for (int i = 0; i < argc; i++) {
std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);
std::transform(argv[i], argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);
args[i] = [=]{ return chk; }
}

bool B = true;
while(out()(args[1])().fillBuffer(B))
B = !B;
return 0;
}