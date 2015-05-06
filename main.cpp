#include <iostream>
#include <functional>
#include "includes.h"

psl::Chunk<char> chr2Chunk(char chr) { psl::Chunk<char> c([=]{return chr;}); return c; }




psl::Chunk<std::function<std::vector<psl::Chunk<psl::Signal>>()>> out;

int main(int argc, char **argv) {
psl::set(out, psl::toChunk([&]() {
return [&]() {
 return psl::toChunk([&]() {
return psl::toVector({psl::toChunk([&]() {
return psl::apply(fromWav, psl::toChunk([&]() {
return psl::fromString("audio/dog.wav");
}));
})});
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
auto fc = out()();
psl::Chunk<psl::Signal> writer1(psl::makeWriter(args[argc-1-1+1], fc[argc-2-1+1]));
do {
B = !B;
success = writer1().fillBuffer(B);
} while (success);
return 0;
}